/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.date;

import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Period;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAdjusters;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * {@link DateUtil} Utility class for Date.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public final class DateUtil implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 6871318395797797413L;

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(DateUtil.class);

	/**
	 * Convertie un Timestamp en chaine respectant le format spécifié.
	 * 
	 * @author HaythemBenizid
	 * @param timestamp Timestamp
	 * @param format Chaine de formatage
	 * @return Chaine date au format spécifié.
	 */
	public static String toString(Timestamp timestamp, String format) {

		if ((timestamp != null) && (format != null) && (format.length() > 0)) {
			Date date = timestamp;
			SimpleDateFormat currentdateFormat = new SimpleDateFormat(format);
			return currentdateFormat.format(date);
		}
		else {
			return null;
		}
	}

	/**
	 * calculate the age by given dateOfBirth {@link Date}.
	 * 
	 * @author HaythemBenizid
	 * @param dateOfBirth the date of birth
	 * @return the age
	 */
	public static Integer calculateAge(Date dateOfBirth) {

		Integer age = 0;
		if (dateOfBirth != null) {
			Calendar born = Calendar.getInstance();
			Calendar now = Calendar.getInstance();
			now.setTime(new Date());
			born.setTime(dateOfBirth);
			if (born.after(now)) {
				return -1;
			}
			age = now.get(Calendar.YEAR) - born.get(Calendar.YEAR);
			if (now.get(Calendar.DAY_OF_YEAR) < born.get(Calendar.DAY_OF_YEAR)) {
				age -= 1;
			}

		}
		return age;
	}

	/**
	 * Calculate age end loan.
	 * 
	 * @author hbeji
	 * @param dateOfBirth the date of birth
	 * @param endLoan the end loan
	 * @return the integer
	 */
	public static Integer calculateAgeEndLoan(Date dateOfBirth, Date endLoan) {

		Integer age = 0;
		if (dateOfBirth != null) {
			Calendar born = Calendar.getInstance();
			Calendar now = Calendar.getInstance();
			now.setTime(endLoan);
			born.setTime(dateOfBirth);
			if (born.after(now)) {
				return -1;
			}
			age = now.get(Calendar.YEAR) - born.get(Calendar.YEAR);
			if (now.get(Calendar.DAY_OF_YEAR) < born.get(Calendar.DAY_OF_YEAR)) {
				age -= 1;
			}

		}
		return age;
	}

	/**
	 * Retourne le Timestamp correspondant à la date et l'heure courante.
	 * 
	 * @author HaythemBenizid
	 * @return Timestamp courant.
	 */
	public static Timestamp getCurrentTimestamp() {

		return new Timestamp(System.currentTimeMillis());
	}

	/**
	 * Retourne la date courante sous forme de Date.
	 * 
	 * @author HaythemBenizid
	 * @return Date courante.
	 */
	public static Date getDate() {

		return new Date(System.currentTimeMillis());
	}

	/**
	 * Retourne la date courante sous forme de chaine au format JJ/MM/AAAA.
	 * 
	 * @author HaythemBenizid
	 * @param dateFormat the date format
	 * @return Date courante.
	 */
	public static String getCurrentDate(String dateFormat) {

		Date currentDate = new Date(System.currentTimeMillis());
		return new SimpleDateFormat(dateFormat).format(currentDate);
	}

	/**
	 * Ajoute un nombre de mois à une date.
	 * 
	 * @author HaythemBenizid
	 * @param date the date
	 * @param months the months
	 * @return Nouvelle date.
	 */
	public static Date addMonths(Date date, int months) {

		if (date != null) {
			GregorianCalendar calendar = new GregorianCalendar();
			calendar.setTime(date);
			calendar.add(Calendar.MONTH, months);
			return calendar.getTime();
		}
		else {
			return null;
		}
	}

	/**
	 * Number of years between two dates.
	 * 
	 * @author HaythemBenizid
	 * @param first date
	 * @param last date
	 * @return nbr of years between first date and last date if (first < last), else return 0
	 */
	public static Integer getDiffYears(Date first, Date last) {

		int diff = 0;
		Calendar a = getCalendar(first);
		Calendar b = getCalendar(last);
		if (a.compareTo(b) > 0) {
			a = getCalendar(last);
			b = getCalendar(first);
		}
		diff = b.get(Calendar.YEAR) - a.get(Calendar.YEAR);
		if (a.get(Calendar.MONTH) > b.get(Calendar.MONTH)
				|| (a.get(Calendar.MONTH) == b.get(Calendar.MONTH)
						&& a.get(Calendar.DATE) > b.get(Calendar.DATE))) {
			diff--;
		}
		return diff;
	}

	/**
	 * transform date to calender.
	 * 
	 * @author HaythemBenizid
	 * @param date the date
	 * @return Calender
	 */
	public static Calendar getCalendar(Date date) {

		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		return cal;
	}

	/**
	 * Add days to a date: positive number would increment the days minus number would decrement the
	 * days.
	 * 
	 * @author HaythemBenizid
	 * @param date the date to increment
	 * @param days the number of days to add
	 * @return the incremented date with x days
	 */
	public static Date addDays(Date date, int days) {

		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		cal.add(Calendar.DATE, days);
		return cal.getTime();
	}

	/**
	 * Gets year from date.
	 * 
	 * @author HaythemBenizid
	 * @param date the date
	 * @return Integer Value of YEAR from given Date
	 */
	public static Integer getYearFromDate(Date date) {

		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		return cal.get(Calendar.YEAR);
	}

	/**
	 * Gets month from date.
	 * 
	 * @author HaythemBenizid
	 * @param date the date
	 * @return Integer Value of MONTH from given Date
	 */
	public static Integer getMonthFromDate(Date date) {

		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		return cal.get(Calendar.MONTH) + 1;
	}

	/**
	 * Gets day from date.
	 * 
	 * @author HaythemBenizid
	 * @param date the date
	 * @return Integer Value of DAY_OF_MONTH from given Date
	 */
	public static Integer getDayFromDate(Date date) {

		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		return cal.get(Calendar.DAY_OF_MONTH);
	}

	/**
	 * get last Date of year by given Date.
	 * 
	 * @author HaythemBenizid
	 * @param date the date
	 * @return {@link Date}
	 */
	public static Date getLastDateOfYear(Date date) {

		int year = getYearFromDate(date);
		Calendar calendarEnd = Calendar.getInstance();
		calendarEnd.set(Calendar.YEAR, year);
		calendarEnd.set(Calendar.MONTH, 11);
		calendarEnd.set(Calendar.DAY_OF_MONTH, 31);

		// returning the last date
		return calendarEnd.getTime();
	}

	/**
	 * get last Date of month by given Date. The Output is in the format YYYY-mm-jj
	 * 
	 * @author HaythemBenizid
	 * @param date the date
	 * @return {@link Date} in the format YYYY-mm-jj
	 */
	public static Date getLastDateOfMonth(Date date) {

		LocalDate now = new java.sql.Date(date.getTime()).toLocalDate();
		LocalDate lastDay = now.with(TemporalAdjusters.lastDayOfMonth());

		// returning the last date
		return java.sql.Date.valueOf(lastDay);
	}

	/**
	 * Get first Date of month by given Date. The Output is in the format YYYY-mm-jj
	 *
	 * @author HaythemBenizid
	 * @param date the date
	 * @return {@link Date} in the format YYYY-mm-jj
	 */
	public static Date getFirstDateOfMonth(Date date) {

		LocalDate now = new java.sql.Date(date.getTime()).toLocalDate();
		LocalDate firstDay = now.with(TemporalAdjusters.firstDayOfMonth());

		// returning the first date
		return java.sql.Date.valueOf(firstDay);
	}

	/**
	 * get last Date of year by given year.
	 * 
	 * @author HaythemBenizid
	 * @param year the year
	 * @return {@link Date}
	 */
	public static Date getLastDateOfYear(Integer year) {

		Calendar calendarEnd = Calendar.getInstance();
		calendarEnd.set(Calendar.YEAR, year);
		calendarEnd.set(Calendar.MONTH, 11);
		calendarEnd.set(Calendar.DAY_OF_MONTH, 31);

		// returning the last date
		return calendarEnd.getTime();
	}

	/**
	 * get Difference Between Two Dates In Months.
	 * 
	 * @author HaythemBenizid
	 * @param startDate the start date
	 * @param endDate the end date
	 * @return {@link Integer}
	 */
	public static Integer getNumberOfMonthsBetweenDates(Date startDate, Date endDate) {

		Period period = Period.between(convertToLocalDateViaInstant(startDate),
				convertToLocalDateViaInstant(endDate));
		return period.getMonths() + (period.getYears() * 12);
	}

	/**
	 * get Difference Between Two Dates In Years.
	 * 
	 * @author HaythemBenizid
	 * @param startDate the start date
	 * @param endDate the end date
	 * @return {@link Integer}
	 */
	public static Integer getDifferenceBetweenTwoDatesInYears(Date startDate, Date endDate) {

		Calendar startCalendar = new GregorianCalendar();
		startCalendar.setTime(startDate);
		Calendar endCalendar = new GregorianCalendar();
		endCalendar.setTime(endDate);

		return endCalendar.get(Calendar.YEAR) - startCalendar.get(Calendar.YEAR);
	}

	/**
	 * return the biggest date value from list of dates.
	 * 
	 * @author HaythemBenizid
	 * @param dates array list of dates
	 * @return the higher value of dates
	 */
	public static Date maxDateFromListDates(List<Date> dates) {

		try {
			if (!dates.isEmpty()) {
				return Collections.max(dates);
			}
			else {
				return null;
			}
		}
		catch (Exception e) {
			logger.error("Failed to extract the maximum element of the given collection {}",
					e.getMessage());
			return null;
		}
	}

	/**
	 * return the lower date value from list of dates.
	 * 
	 * @author HaythemBenizid
	 * @param dates array list of dates
	 * @return the lower value of dates
	 */
	public static Date minDateFromListDates(List<Date> dates) {

		try {
			if (!dates.isEmpty()) {
				return Collections.min(dates);
			}
			else {
				return null;
			}
		}
		catch (Exception e) {
			logger.error("Failed to extract the minimum element of the given collection : {}",
					e.getMessage());
			return null;
		}
	}

	/**
	 * Adds the years.
	 * 
	 * @author HaythemBenizid
	 * @param date the date
	 * @param years the years
	 * @return the date
	 */
	public static Date addYears(Date date, int years) {

		if (date != null) {
			GregorianCalendar calendar = new GregorianCalendar();
			calendar.setTime(date);
			calendar.add(Calendar.YEAR, years);
			return calendar.getTime();
		}
		else {
			return null;
		}
	}

	/**
	 * get first Date of year by given Date.
	 * 
	 * @author HaythemBenizid
	 * @param date the date
	 * @return {@link Date}
	 */
	public static Date getFirstDateOfYear(Date date) {

		int year = getYearFromDate(date);
		Calendar calendarStart = Calendar.getInstance();
		calendarStart.set(Calendar.YEAR, year);
		calendarStart.set(Calendar.DAY_OF_YEAR, 1);
		// returning the first date
		return calendarStart.getTime();
	}

	/**
	 * Calculate days between two dates. Count number of days between start date and end date. Java
	 * doesn’t offer the possibility to calculate the number of days using two inclusive dates. If
	 * we want to count the number of days between two dates including them, we have to add 1 to the
	 * result or add 1 day to the end date
	 *
	 * @author HaythemBenizid
	 * @param startDate the start date
	 * @param endDate the end date
	 * @return the long
	 */
	public static Long calculateDaysBetweenTwoDates(Date startDate, Date endDate) {

		return ChronoUnit.DAYS.between(DateUtil.convertToLocalDateViaInstant(startDate),
				DateUtil.convertToLocalDateViaInstant(endDate)) + 1;
	}

	/**
	 * convert date {@link Date} to {@link java.time.LocalDate} local date.
	 *
	 * @author HaythemBenizid
	 * @param dateToConvert date to be converted {@link Date}
	 * @return the converdate {@link java.time.LocalDate}
	 */
	public static LocalDate convertToLocalDateViaInstant(Date dateToConvert) {

		return dateToConvert.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
	}

	/**
	 * Format the giving date to the giving format.
	 *
	 * @author HaythemBenizid
	 * @param date the date object to format
	 * @param format the format
	 * @return the formatted date string
	 */
	public static String formatDate(Date date, String format) {

		if ((date != null) && (format != null) && (format.length() > 0)) {
			return new SimpleDateFormat(format).format(date);
		}
		else {
			return null;
		}
	}

	/**
	 * Setting Current Time To given Date. Example : given date = Fri Jun 26 00:00:00 WAT 2020 =>
	 * returned date = Fri Jun 26 15:11:32 WAT 2020
	 *
	 * @author HaythemBenizid
	 * @param date the date
	 * @return the date
	 */
	public static Date setCurrentTimeToDate(Date date) {

		LocalDateTime convertedDate = new java.sql.Timestamp(date.getTime()).toLocalDateTime();
		Calendar calendar = Calendar.getInstance();
		calendar.set(convertedDate.getYear(), convertedDate.getMonthValue() - 1,
				convertedDate.getDayOfMonth());
		return calendar.getTime();
	}

	/**
	 * reset Time Date. Example : given date = Fri Jun 26 15:11:32 WAT 2020 => returned date = Fri
	 * Jun 26 00:00:00 WAT 2020
	 *
	 * @author MoezMhiri
	 * @param date the date
	 * @return the date
	 */
	public static Date resetTimeDate(Date date) {

		LocalDateTime convertedDate = new java.sql.Timestamp(date.getTime()).toLocalDateTime();
		Calendar calendar = Calendar.getInstance();
		calendar.set(convertedDate.getYear(), convertedDate.getMonthValue() - 1,
				convertedDate.getDayOfMonth(), 0, 0, 0);
		return calendar.getTime();
	}

	/**
	 * Setting Current Time To given Date. Example : given date = Fri Jun 26 00:00:00 WAT 2020 =>
	 * Setting ResigningDate Time To given Date. Example : given date = Fri Jun 27 00:00:00 WAT 2020
	 * => returned True
	 *
	 * @param currentDate the current date
	 * @param resigningDate the resigning date
	 * @return the boolean
	 */
	public static Boolean isDateOfInterestValid(Date currentDate, Date resigningDate) {

		long diff = currentDate.getTime() - resigningDate.getTime();
		long diffDays = (diff / (24 * 1000 * 60 * 60));
		return !(diff >= 0 && diffDays >= 0);
	}

	/**
	 * Date to date time.
	 *
	 * @author MoezMhiri
	 * @param date the date
	 * @param time the time
	 * @return the timestamp
	 */
	public static Timestamp dateToDateTime(Date date, String time) {

		SimpleDateFormat mdyFormat = new SimpleDateFormat("yyyy-MM-dd");
		String dateFormat = mdyFormat.format(date);
		String dateTime = dateFormat + " " + time;

		return Timestamp.valueOf(dateTime);
	}
}
