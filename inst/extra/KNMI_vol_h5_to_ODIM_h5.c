/******************************************************************************/
/*This program converts KNMI HDF5 radar volume data files to ODIM HDF5 radar  */
/*volume data files.                                                          */
/******************************************************************************/

/*Program: KNMI_vol_h5_to_ODIM_h5.c*/
/*Author: Hidde Leijnse (KNMI)*/
/*Date: January 2012*/
/*Version: February 2018*/

/*Modified by: Bart Hoekstra (UvA), January 2019*/
/*Modifications are aimed at retaining all raw data from original KNMI HDF5 */
/*radar volume data files and are clearly commented with the MODIFIED keyword */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <hdf5.h>
#include <hdf5_hl.h>

/******************************************************************************/
/*Definition of standard parameters.                                          */
/******************************************************************************/
#define STRLEN       (128)               /*Length of all strings used.*/
#define NDSETX       (128)
#define NMOMENTX     (64)

struct scanmeta {
	int date;             /*Date of scan data in YYYYMMDD.*/
	int time;             /*Time of scan data in HHMMSS.*/
	float elev;           /*Elevation of scan in deg.*/
	int nrang;            /*Number of range bins in scan.*/
	int nazim;            /*Number of azimuth rays in scan.*/
	float rscale;         /*Size of range bins in scan in km.*/
	float ascale;         /*Size of azimuth steps in scan in deg.*/
	int azim0;            /*Ray number with which radar scan started.*/
	int missing;          /*Missing value of quantity contained by scan.*/
	float PRFh;           /*High PRF used for scan in Hz.*/
	float PRFl;           /*Low PRF used for scan in Hz.*/
	float pulse;          /*Pulse length in microsec.*/
	float radcnst;        /*Radar constant in dB.*/
	float radcnst_H;      /*Radar constant for H channel in dB.*/
	float radcnst_V;      /*Radar constant for V channel in dB.*/
	float txnom;          /*Nominal maximum TX power in kW.*/
	float antvel;         /*Antenna velocity in deg/s.*/
	float lon;            /*Longitude of radar in deg.*/
	float lat;            /*Latitude of radar in deg.*/
	float Z_offset;       /*Offset value of quantity contained by scan.*/
	float Z_scale;        /*Scale of value of quantity contained by scan.*/
};
typedef struct scanmeta SCANMETA;

int read_h5_scan_meta(hid_t file, int iscan, SCANMETA *meta);
void string2datetime(char *string, int *date, int *time);
herr_t H5LTmake_dataset_zip(hid_t loc_id, char *dset_name, int rank, hsize_t *dims, hid_t tid, void *data);

int main(int argc, char **argv)
{
	hid_t h5_in, h5_out;
	char object_in[STRLEN], object[STRLEN], attribute_name[STRLEN], string[STRLEN], id_string_DeBilt[STRLEN], id_string_DenHelder[STRLEN], id_string_Herwijnen[STRLEN], moment_names_KNMI[NMOMENTX][STRLEN], moment_names_ODIM[NMOMENTX][STRLEN];
	double x_double, lat, lon, lon_DenHelder, height_DeBilt, height_DenHelder, height_Herwijnen, wavelength, beamwidth, gain, offset;
	int correct_calib_error, correct_lon_DH, i, j, t, Nscan, radnum, data_cnt, N_data, nodata;
	float radloc[2];
	long x_long;
	SCANMETA meta;

	/*Set values.*/
	correct_calib_error = 0;
	correct_lon_DH = 0;
	height_DeBilt = 44;
	height_DenHelder = 51;
	height_Herwijnen = 27.7;
	sprintf(id_string_DeBilt, "RAD:NL50,NOD:nldbl,PLC:De Bilt");
	sprintf(id_string_DenHelder, "RAD:NL51,NOD:nldhl,PLC:Den Helder");
	sprintf(id_string_Herwijnen, "RAD:NL52,NOD:nlhrw,PLC:Herwijnen");
	lon_DenHelder = 4.789997;
	wavelength = 5.326;
	beamwidth = 0.905;

	N_data = 18; /* MODIFIED */
	sprintf(moment_names_KNMI[0], "Z");
	sprintf(moment_names_KNMI[1], "uZ");
	sprintf(moment_names_KNMI[2], "Zv");
	sprintf(moment_names_KNMI[3], "uZv");
	sprintf(moment_names_KNMI[4], "V");
	sprintf(moment_names_KNMI[5], "W");
	sprintf(moment_names_KNMI[6], "RhoHV");
	sprintf(moment_names_KNMI[7], "KDP");
	sprintf(moment_names_KNMI[8], "PhiDP");
	sprintf(moment_names_KNMI[9], "CPA");
	sprintf(moment_names_KNMI[10], "CCOR");
	sprintf(moment_names_KNMI[11], "SQI");
	sprintf(moment_names_KNMI[12], "CCORv"); /* MODIFIED */
	sprintf(moment_names_KNMI[13], "CPAv"); /* MODIFIED */
	sprintf(moment_names_KNMI[14], "SQIv"); /* MODIFIED */
	sprintf(moment_names_KNMI[15], "uPhiDP"); /* MODIFIED */
	sprintf(moment_names_KNMI[16], "Vv"); /* MODIFIED */
	sprintf(moment_names_KNMI[17], "Wv"); /* MODIFIED */
	sprintf(moment_names_ODIM[0], "DBZH");
	sprintf(moment_names_ODIM[1], "TH");
	sprintf(moment_names_ODIM[2], "DBZV");
	sprintf(moment_names_ODIM[3], "TV");
	sprintf(moment_names_ODIM[4], "VRADH");
	sprintf(moment_names_ODIM[5], "WRADH");
	sprintf(moment_names_ODIM[6], "RHOHV");
	sprintf(moment_names_ODIM[7], "KDP");
	sprintf(moment_names_ODIM[8], "PHIDP");
	sprintf(moment_names_ODIM[9], "CPAH");
	sprintf(moment_names_ODIM[10], "CCORH");
	sprintf(moment_names_ODIM[11], "SQIH");
	sprintf(moment_names_ODIM[12], "CCORV"); /* MODIFIED */
	sprintf(moment_names_ODIM[13], "CPAV"); /* MODIFIED */
	sprintf(moment_names_ODIM[14], "SQIV"); /* MODIFIED */
	sprintf(moment_names_ODIM[15], "PHIDPU"); /* MODIFIED */
	sprintf(moment_names_ODIM[16], "VRADV"); /* MODIFIED */
	sprintf(moment_names_ODIM[17], "WRADV"); /* MODIFIED */



	/*Check number of input arguments.*/
	if (argc < 3)
	{
		fprintf(stderr, "Usage: %s ODIM_file.h5 KNMI_input_file.h5\n", argv[0]);
		exit(1);
	}

	/*Open input file.*/
	if ((h5_in = H5Fopen(argv[2], H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
	{
		fprintf(stderr, "Error: KNMI HDF5 input file %s could not be opened!\n", argv[2]);
		exit(2);
	}

	/*Open output file.*/
	if ((h5_out = H5Fcreate(argv[1], H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	{
		fprintf(stderr, "Error: ODIM HDF5 output file %s could not be opened!\n", argv[1]);
		H5Fclose(h5_in);
		exit(3);
	}

	/*Read KNMI volume data file.*/
	H5LTget_attribute_int(h5_in, "/overview", "number_scan_groups", &Nscan);
	/*meta = (SCANMETA *) malloc(Nscan * sizeof(SCANMETA));
	for (i = 0; i < Nscan; i++)
	{
		if (read_h5_scan(h5_in, i + 1, &(meta[i])) == 0)
		{
			Nscan = i;
			break;
		}
	}*/

	/*Read radar information.*/
	H5LTget_attribute_string(h5_in, "/radar1", "radar_name", string);
	if ((strcmp(string, "DeBilt") == 0) || (strcmp(string, "De_Bilt") == 0)) radnum = 60;
	if ((strcmp(string, "DenHelder") == 0) || (strcmp(string, "Den_Helder") == 0)) radnum = 61;
	if (strcmp(string, "Herwijnen") == 0) radnum = 62;
	H5LTget_attribute_float(h5_in, "/radar1", "radar_location", radloc);
	lon = (double) radloc[0];
	lat = (double) radloc[1];
	if ((radnum == 61) && (correct_lon_DH == 1)) lon = lon_DenHelder;
	read_h5_scan_meta(h5_in, 1, &meta);

	/*Create root attribute.*/
	H5LTset_attribute_string(h5_out, "/", "Conventions", "ODIM_H5/V2_2");

	/*Create and populate top-level what-group.*/
	H5Gcreate(h5_out, "/what", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5LTset_attribute_string(h5_out, "/what", "object", "PVOL");
	H5LTset_attribute_string(h5_out, "/what", "version", "H5rad 2.2");
	sprintf(string, "%08d", meta.date);
	H5LTset_attribute_string(h5_out, "/what", "date", string);
	sprintf(string, "%06d", meta.time);
	H5LTset_attribute_string(h5_out, "/what", "time", string);
	if (radnum == 60) H5LTset_attribute_string(h5_out, "/what", "source", id_string_DeBilt);
	if (radnum == 61) H5LTset_attribute_string(h5_out, "/what", "source", id_string_DenHelder);
	if (radnum == 62) H5LTset_attribute_string(h5_out, "/what", "source", id_string_Herwijnen);


	/*Create and populate top-level where-group.*/
	H5Gcreate(h5_out, "/where", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5LTset_attribute_double(h5_out, "/where", "lon", &lon, 1);
	H5LTset_attribute_double(h5_out, "/where", "lat", &lat, 1);
	if (radnum == 60) x_double = height_DeBilt;
	if (radnum == 61) x_double = height_DenHelder;
	if (radnum == 62) x_double = height_Herwijnen;
	H5LTset_attribute_double(h5_out, "/where", "height", &x_double, 1);

	/*Create and populate top-level how-group.*/
	H5Gcreate(h5_out, "/how", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5LTset_attribute_double(h5_out, "/how", "wavelength", &wavelength, 1);
	H5LTset_attribute_double(h5_out, "/how", "beamwidth", &beamwidth, 1);

	/*Create and populate different scan groups.*/
	for (i = 0; i < Nscan; i++)
	{
		read_h5_scan_meta(h5_in, i + 1, &meta);

		sprintf(object, "/dataset%d", i + 1);
		H5Gcreate(h5_out, object, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

		/*Create and populate what-group.*/
		sprintf(object, "/dataset%d/what", i + 1);
		H5Gcreate(h5_out, object, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5LTset_attribute_string(h5_out, object, "product", "SCAN");
		sprintf(string, "%08d", meta.date);
		H5LTset_attribute_string(h5_out, object, "startdate", string);
		sprintf(string, "%06d", meta.time);
		H5LTset_attribute_string(h5_out, object, "starttime", string);
		sprintf(string, "%08d", meta.date);
		H5LTset_attribute_string(h5_out, object, "enddate", string);
		t = meta.time % 100 + ((int) (360.0 / meta.antvel));
		if (t >= 60) t += 40;
		t += (meta.time / 100) * 100;
		sprintf(string, "%06d", t);
		H5LTset_attribute_string(h5_out, object, "endtime", string);

		/*Create and populate where-group.*/
		sprintf(object, "/dataset%d/where", i + 1);
		H5Gcreate(h5_out, object, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		x_double = (double) meta.elev;
		H5LTset_attribute_double(h5_out, object, "elangle", &x_double, 1);
		x_long = (long) meta.nrang;
		H5LTset_attribute_long(h5_out, object, "nbins", &x_long, 1);
		x_double = 0.0;
		H5LTset_attribute_double(h5_out, object, "rstart", &x_double, 1);
		x_double = (double) meta.rscale * 1000.0;
		H5LTset_attribute_double(h5_out, object, "rscale", &x_double, 1);
		x_long = (long) meta.nazim;
		H5LTset_attribute_long(h5_out, object, "nrays", &x_long, 1);
		x_long = (long) meta.azim0;
		H5LTset_attribute_long(h5_out, object, "a1gate", &x_long, 1);

		/*Create and populate how-group.*/
		sprintf(object, "/dataset%d/how", i + 1);
		H5Gcreate(h5_out, object, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		sprintf(object_in, "scan%d/scan_V_data", i + 1);
		if (H5Lexists(h5_in, object_in, H5P_DEFAULT))
		{

			sprintf(object_in, "scan%d/calibration", i + 1);
			H5LTget_attribute_string(h5_in, object_in, "calibration_V_formulas", string);
			sscanf(string, "GEO=%lf*PV+%lf", &gain, &offset);
			x_double = -1.0 * offset;
			H5LTset_attribute_double(h5_out, object, "NI", &x_double, 1);
		}
		x_double = (double) meta.antvel / 6.0;
		H5LTset_attribute_double(h5_out, object, "rpm", &x_double, 1);
		x_double = (double) meta.PRFh;
		H5LTset_attribute_double(h5_out, object, "highprf", &x_double, 1);
		x_double = (double) meta.PRFl;
		H5LTset_attribute_double(h5_out, object, "lowprf", &x_double, 1);
		x_double = (double) meta.pulse;
		H5LTset_attribute_double(h5_out, object, "pulsewidth", &x_double, 1);
		x_double = (double) meta.radcnst_H;
		H5LTset_attribute_double(h5_out, object, "radconstH", &x_double, 1);
		x_double = (double) meta.radcnst_V;
		H5LTset_attribute_double(h5_out, object, "radconstV", &x_double, 1);
		x_double = (double) meta.txnom;
		H5LTset_attribute_double(h5_out, object, "nomTXpower", &x_double, 1);

		data_cnt = 0;
		for (j = 0; j < N_data; j++)
		{
			sprintf(object_in, "scan%d/scan_%s_data", i + 1, moment_names_KNMI[j]);
			if (H5Lexists(h5_in, object_in, H5P_DEFAULT))
			{
				/*Create data group.*/
				data_cnt += 1;
				sprintf(object, "/dataset%d/data%d", i + 1, data_cnt);
				H5Gcreate(h5_out, object, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

				/*Copy dataset.*/
				sprintf(object, "/dataset%d/data%d/data", i + 1, data_cnt);
				H5Ocopy(h5_in, object_in, h5_out, object, H5P_DEFAULT, H5P_DEFAULT);

				/*Create and populate what group.*/
				sprintf(object, "/dataset%d/data%d/what", i + 1, data_cnt);
				H5Gcreate(h5_out, object, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
				H5LTset_attribute_string(h5_out, object, "quantity", moment_names_ODIM[j]);
				sprintf(object_in, "scan%d/calibration", i + 1);
				sprintf(attribute_name, "calibration_%s_formulas", moment_names_KNMI[j]);
				H5LTget_attribute_string(h5_in, object_in, attribute_name, string);
				sscanf(string, "GEO=%lf*PV+%lf", &gain, &offset);
				H5LTset_attribute_double(h5_out, object, "gain", &gain, 1);
				H5LTset_attribute_double(h5_out, object, "offset", &offset, 1);
				H5LTget_attribute_int(h5_in, object_in, "calibration_missing_data", &nodata);
				x_double = (double) nodata;
				H5LTset_attribute_double(h5_out, object, "nodata", &x_double, 1);
				H5LTset_attribute_double(h5_out, object, "undetect", &x_double, 1);

			}
		}
	}

	/*Close files.*/
	H5Fclose(h5_in);
	H5Fclose(h5_out);

	/*End of program.*/
	return 0;
}



int read_h5_scan_meta(hid_t file, int iscan, SCANMETA *meta)
{
	char object[STRLEN], string[STRLEN];
	int Nscan = 0;

	/*Checking requested scan number.*/
	H5LTget_attribute_int(file, "/overview","number_scan_groups", &Nscan);
	if ((iscan < 1) || (iscan > Nscan))
	{
		fprintf(stderr, "Requested scan is not available in this file\n");
		return 0;
	}

	/*Reading of metadata from scan group in file.*/
	sprintf(object, "/scan%d", iscan);
	H5LTget_attribute_string(file, object, "scan_datetime", string);
	string2datetime(string, &((*meta).date), &((*meta).time));
	H5LTget_attribute_float(file, object, "scan_elevation", &((*meta).elev));
	H5LTget_attribute_int(file, object, "scan_number_range", &((*meta).nrang));
	H5LTget_attribute_int(file, object, "scan_number_azim", &((*meta).nazim));
	H5LTget_attribute_float(file, object, "scan_range_bin", &((*meta).rscale));
	H5LTget_attribute_float(file, object, "scan_azim_bin", &((*meta).ascale));
	H5LTget_attribute_int(file, object, "scan_start_azim", &((*meta).azim0));
	H5LTget_attribute_float(file, object, "scan_low_PRF", &((*meta).PRFl));
	H5LTget_attribute_float(file, object, "scan_high_PRF", &((*meta).PRFh));
	H5LTget_attribute_float(file, object, "scan_pulse_width", &((*meta).pulse));
	H5LTget_attribute_float(file, object, "scan_radar_constant", &((*meta).radcnst));
	H5LTget_attribute_float(file, object, "scan_radar_constant_H", &((*meta).radcnst_H));
	H5LTget_attribute_float(file, object, "scan_radar_constant_V", &((*meta).radcnst_V));
	H5LTget_attribute_float(file, object, "scan_TX_power_nom", &((*meta).txnom));
	H5LTget_attribute_float(file, object, "scan_antenna_velocity", &((*meta).antvel));

	/*Returning.*/
	return 1;
}


/******************************************************************************/
/*This function converts a string according to the KNMI HDF5 specification to */
/*a date (yyyymmdd) and time (hhmmss).                                        */
/******************************************************************************/
void string2datetime(char *string, int *date, int *time)
{
	char months[13][4]={"XXX", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"};
	char month[4];
	int yy=0, mon=0, dd=0, hh=0, mm=0, ss=0, m;

	/*Extraction of year, month, day, etc from string.*/
	sscanf(string, "%d-%[^-]-%d;%d:%d:%d", &dd, month, &yy, &hh, &mm, &ss);
	for (m = 1; m <= 12; m++)
	{
		if (strncmp(month, months[m], 3) == 0) mon = m;
	}

	/*Returning date and time in integer format.*/
	(*date) = 10000 * yy + 100 * mon + dd;
	(*time) = 10000 * hh + 100 * mm + ss;
}


/******************************************************************************/
/* Function: H5LTmake_dataset                                                 */
/* Purpose: Creates and writes a dataset of a type tid                        */
/* Return: Success: 0, Failure: -1                                            */
/* Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu                               */
/* Date: March 19, 2001                                                       */
/* Comments:                                                                  */
/* Modifications: Datasets are compressed  (Iwan Holleman, KNMI)              */
/******************************************************************************/

herr_t H5LTmake_dataset_zip(hid_t loc_id, char *dset_name, int rank, hsize_t *dims, hid_t tid, void *data)
{
	#define H5ZIPLEVEL (6)
	hid_t did, sid, pid;

	/*Create the data space for the dataset.*/
	if ((sid = H5Screate_simple(rank, dims, NULL)) < 0) return -1;

	/*Create the property list for zipped datasets.*/
	pid = H5Pcreate(H5P_DATASET_CREATE);
	H5Pset_chunk(pid, rank, dims);
	H5Pset_deflate(pid, H5ZIPLEVEL);

	/*Create the dataset.*/
	if ((did = H5Dcreate(loc_id, dset_name, tid, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) goto out;

	/*Write the dataset only if there is data to write*/
	if (data)
	{
		if (H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0) goto out;
	}

	/*End access to the dataset and release resources used by it.*/
	if (H5Dclose(did) < 0) return -1;

	/*Terminate access to the data space. */
	if (H5Sclose(sid) < 0) return -1;

	/*End access to the property list.*/
	if (H5Pclose(pid) < 0) return -1;
	return 0;

	out:
		H5Dclose(did);
		H5Sclose(sid);
		H5Sclose(pid);
		return -1;
}
