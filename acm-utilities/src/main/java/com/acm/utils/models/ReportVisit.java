/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * {@link ReportVisit} class.
 *
 * @author Yesser Somai
 * @since 0.2.0
 */
@Entity
@Table(name = "ACM_REPORT_VISIT")
public class ReportVisit extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2509717374472022153L;

	/** The id of Report visit. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_REPORT_VISIT", unique = true, nullable = false)
	private Long idReportVisit;

	/** The Description Report. */
	@Column(name = "DESCRIPTION", nullable = false)
	private String description;

	/** The planned visit. */
	@Column(name = "PLANNED_VISIT", nullable = false)
	private Date plannedVisit;

	/** The username visitor. */
	@Column(name = "VISIT_BY", nullable = false)
	private String visitBy;

	/** The comment. */
	@Column(name = "COMMENT", nullable = false)
	private String comment;

	/** The loan. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_LOAN")
	private Loan loan;

	/**
	 * Instantiates a new acm rapport visit.
	 */
	public ReportVisit() {

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
	 * Gets the loan.
	 *
	 * @return the loan
	 */
	public Loan getLoan() {

		return loan;
	}

	/**
	 * Sets the loan.
	 *
	 * @param loan the loan to set
	 */
	public void setLoan(Loan loan) {

		this.loan = loan;
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
	 * Gets the visitBy.
	 *
	 * @return the visitBy
	 */
	public String getvisitBy() {

		return visitBy;
	}

	/**
	 * Sets the visitBy.
	 *
	 * @param visitBy the visitBy to set
	 */
	public void setvisitBy(String visitBy) {

		this.visitBy = visitBy;
	}

}
