/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.util.Date;

import org.dozer.Mapping;

/**
 * {@link ReportVisitDTO } class.
 *
 * @author YesserSomai
 * @since 0.4.0
 */
public class ReportVisitDTO extends GenericDTO {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 6516374926558197856L;

	/** The id of Report visit. */
	private Long idReportVisit;

	/** The Description Report. */
	private String description;

	/** The username visitor. */
	private String visitBy;

	/** The planned visit. */
	private Date plannedVisit;

	/** The comment. */
	private String comment;

	/** The insert by. */
	private String insertBy;

	/** The date insertion. */
	private Date dateInsertion;

	/** The id loan. */
	@Mapping("loan.idLoan")
	private Long idLoan;

	/**
	 * Instantiates a new acm rapport visit.
	 */
	public ReportVisitDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the id report visit.
	 *
	 * @return the id report visit
	 */
	public Long getIdReportVisit() {

		return idReportVisit;
	}

	/**
	 * Sets the id report visit.
	 *
	 * @param idReportVisit the new id report visit
	 */
	public void setIdReportVisit(Long idReportVisit) {

		this.idReportVisit = idReportVisit;
	}

	/**
	 * Gets the description.
	 *
	 * @return the description
	 */
	public String getDescription() {

		return description;
	}

	/**
	 * Sets the description.
	 *
	 * @param description the new description
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the id loan.
	 *
	 * @return the idLoan
	 */
	public Long getIdLoan() {

		return idLoan;
	}

	/**
	 * Sets the id loan.
	 *
	 * @param idLoan the idLoan to set
	 */
	public void setIdLoan(Long idLoan) {

		this.idLoan = idLoan;
	}

	/**
	 * Gets the planned visit.
	 *
	 * @return the plannedVisit
	 */
	public Date getPlannedVisit() {

		return plannedVisit;
	}

	/**
	 * Sets the planned visit.
	 *
	 * @param plannedVisit the plannedVisit to set
	 */
	public void setPlannedVisit(Date plannedVisit) {

		this.plannedVisit = plannedVisit;
	}

	/**
	 * Gets the comment.
	 *
	 * @return the comment
	 */
	public String getComment() {

		return comment;
	}

	/**
	 * Sets the comment.
	 *
	 * @param comment the comment to set
	 */
	public void setComment(String comment) {

		this.comment = comment;
	}

	/**
	 * Gets the insert by.
	 *
	 * @return the insertBy
	 */
	public String getInsertBy() {

		return insertBy;
	}

	/**
	 * Sets the insert by.
	 *
	 * @param insertBy the insertBy to set
	 */
	public void setInsertBy(String insertBy) {

		this.insertBy = insertBy;
	}

	/**
	 * Gets the visit by.
	 * 
	 * @return the visitBy
	 */
	public String getVisitBy() {

		return visitBy;
	}

	/**
	 * Sets the visit by.
	 *
	 * @param visitBy the visitBy to set
	 */
	public void setVisitBy(String visitBy) {

		this.visitBy = visitBy;
	}

	/**
	 * Gets the date insertion.
	 *
	 * @return the dateInsertion
	 */
	public Date getDateInsertion() {

		return dateInsertion;
	}

	/**
	 * Sets the date insertion.
	 *
	 * @param dateInsertion the dateInsertion to set
	 */
	public void setDateInsertion(Date dateInsertion) {

		this.dateInsertion = dateInsertion;
	}

}
