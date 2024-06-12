package com.acm.utils.models;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

// TODO: Auto-generated Javadoc
/**
 * The Class Planing.
 */
@Entity
@Table(name = "ACM_PLANNING_CRON")
public class PlaningStep {

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_PLANNING_CRON", unique = true, nullable = false)
	private Long id;

	/** The day rep. */
	@Column(name = "DAY_REP")
	private Integer dayRep;

	/** The category. */
	@Column(name = "CATEGORY")
	private String category;

	/** The lst day. */
	@Column(name = "LST_DAY")
	private String lstDay;

	/** The date of rep. */
	@Column(name = "DATE_OF_REP")
	private Date dateOfRep;

	/** The begin or end month. */
	@Column(name = "BEGIN_OR_END_MONTH")
	private String beginOrEndMonth;

	/** The date of month. */
	@Column(name = "DATE_OF_MONTH")
	private Integer dateOfMonth;

	/** The selected day. */
	@Column(name = "SELECTED_DAY")
	private String selectedDay;

	/** The nbr month. */
	@Column(name = "NBR_MONTH")
	private String nbrMonth;

	/** The nbr month. */
	@Column(name = "MONTH")
	private String month;

	/**
	 * Gets the day rep.
	 *
	 * @return the day rep
	 */
	public Integer getDayRep() {

		return dayRep;
	}

	/**
	 * Sets the day rep.
	 *
	 * @param dayRep the new day rep
	 */
	public void setDayRep(Integer dayRep) {

		this.dayRep = dayRep;
	}

	/**
	 * Gets the category.
	 *
	 * @return the category
	 */
	public String getCategory() {

		return category;
	}

	/**
	 * Sets the category.
	 *
	 * @param category the new category
	 */
	public void setCategory(String category) {

		this.category = category;
	}

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the lst day.
	 *
	 * @return the lst day
	 */
	public String getLstDay() {

		return lstDay;
	}

	/**
	 * Sets the lst day.
	 *
	 * @param lstDay the new lst day
	 */
	public void setLstDay(String lstDay) {

		this.lstDay = lstDay;
	}

	/**
	 * Gets the date of rep.
	 *
	 * @return the date of rep
	 */
	public Date getDateOfRep() {

		return dateOfRep;
	}

	/**
	 * Sets the date of rep.
	 *
	 * @param dateOfRep the new date of rep
	 */
	public void setDateOfRep(Date dateOfRep) {

		this.dateOfRep = dateOfRep;
	}

	/**
	 * Gets the begin or end month.
	 *
	 * @return the begin or end month
	 */
	public String getBeginOrEndMonth() {

		return beginOrEndMonth;
	}

	/**
	 * Sets the begin or end month.
	 *
	 * @param beginOrEndMonth the new begin or end month
	 */
	public void setBeginOrEndMonth(String beginOrEndMonth) {

		this.beginOrEndMonth = beginOrEndMonth;
	}

	/**
	 * Gets the date of month.
	 *
	 * @return the date of month
	 */
	public Integer getDateOfMonth() {

		return dateOfMonth;
	}

	/**
	 * Sets the date of month.
	 *
	 * @param dateOfMonth the new date of month
	 */
	public void setDateOfMonth(Integer dateOfMonth) {

		this.dateOfMonth = dateOfMonth;
	}

	/**
	 * Gets the selected day.
	 *
	 * @return the selected day
	 */
	public String getSelectedDay() {

		return selectedDay;
	}

	/**
	 * Sets the selected day.
	 *
	 * @param selectedDay the new selected day
	 */
	public void setSelectedDay(String selectedDay) {

		this.selectedDay = selectedDay;
	}

	/**
	 * Gets the nbr month.
	 *
	 * @return the nbr month
	 */
	public String getNbrMonth() {

		return nbrMonth;
	}

	/**
	 * Sets the nbr month.
	 *
	 * @param nbrMonth the new nbr month
	 */
	public void setNbrMonth(String nbrMonth) {

		this.nbrMonth = nbrMonth;
	}

	/**
	 * Gets the month.
	 *
	 * @return the month
	 */
	public String getMonth() {

		return month;
	}

	/**
	 * Sets the month.
	 *
	 * @param month the new month
	 */
	public void setMonth(String month) {

		this.month = month;
	}

}
