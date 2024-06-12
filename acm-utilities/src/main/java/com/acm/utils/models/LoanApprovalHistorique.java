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
 * {@link LoanApprovalHistorique} class.
 *
 * @author HaythemBenizid
 * @since 0.6.0
 */
@Entity
@Table(name = "ACM_LOAN_APPROVAL_HISTORIQUE")
public class LoanApprovalHistorique extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7459784443785852009L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_LOAN_APPROVAL_HISTORIQUE", unique = true, nullable = false)
	private Long id;

	/** The loan. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_LOAN")
	private Loan loan;

	/** The approval date. */
	@Column(name = "APPROVAL_DATE", nullable = false)
	private Date approvalDate;

	/** The approval amount. */
	@Column(name = "APPROVAL_AMOUNT", nullable = false)
	private Long approvalAmount;

	/** The approval desicion. */
	@Column(name = "APPROVAL_DESICION", nullable = false)
	private Integer approvalDesicion;

	/** The approval desicion label. */
	@Column(name = "APPROVAL_DESICION_LABEL", nullable = false, length = 256)
	private String approvalDesicionLabel;

	/** The approval note. */
	@Column(name = "APPROVAL_NOTE", nullable = false)
	private String approvalNote;

	/** The approval level. */
	@Column(name = "APPROVAL_LEVEL", nullable = false)
	private Integer approvalLevel;

	/** The approval level label. */
	@Column(name = "APPROVAL_LEVEL_LABEL", nullable = false, length = 256)
	private String approvalLevelLabel;

	/** The approved by. */
	@Column(name = "APPROVED_BY", nullable = false, length = 256)
	private String approvedBy;

	/**
	 * Instantiates a new loan approval historique.
	 */
	public LoanApprovalHistorique() {

		/*
		 * 
		 */
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
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
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
	 * Gets the approval date.
	 *
	 * @return the approvalDate
	 */
	public Date getApprovalDate() {

		return approvalDate;
	}

	/**
	 * Sets the approval date.
	 *
	 * @param approvalDate the approvalDate to set
	 */
	public void setApprovalDate(Date approvalDate) {

		this.approvalDate = approvalDate;
	}

	/**
	 * Gets the approval amount.
	 *
	 * @return the approvalAmount
	 */
	public Long getApprovalAmount() {

		return approvalAmount;
	}

	/**
	 * Sets the approval amount.
	 *
	 * @param approvalAmount the approvalAmount to set
	 */
	public void setApprovalAmount(Long approvalAmount) {

		this.approvalAmount = approvalAmount;
	}

	/**
	 * Gets the approval desicion.
	 *
	 * @return the approvalDesicion
	 */
	public Integer getApprovalDesicion() {

		return approvalDesicion;
	}

	/**
	 * Sets the approval desicion.
	 *
	 * @param approvalDesicion the approvalDesicion to set
	 */
	public void setApprovalDesicion(Integer approvalDesicion) {

		this.approvalDesicion = approvalDesicion;
	}

	/**
	 * Gets the approval desicion label.
	 *
	 * @return the approvalDesicionLabel
	 */
	public String getApprovalDesicionLabel() {

		return approvalDesicionLabel;
	}

	/**
	 * Sets the approval desicion label.
	 *
	 * @param approvalDesicionLabel the approvalDesicionLabel to set
	 */
	public void setApprovalDesicionLabel(String approvalDesicionLabel) {

		this.approvalDesicionLabel = approvalDesicionLabel;
	}

	/**
	 * Gets the approval note.
	 *
	 * @return the approvalNote
	 */
	public String getApprovalNote() {

		return approvalNote;
	}

	/**
	 * Sets the approval note.
	 *
	 * @param approvalNote the approvalNote to set
	 */
	public void setApprovalNote(String approvalNote) {

		this.approvalNote = approvalNote;
	}

	/**
	 * Gets the approval level.
	 *
	 * @return the approvalLevel
	 */
	public Integer getApprovalLevel() {

		return approvalLevel;
	}

	/**
	 * Sets the approval level.
	 *
	 * @param approvalLevel the approvalLevel to set
	 */
	public void setApprovalLevel(Integer approvalLevel) {

		this.approvalLevel = approvalLevel;
	}

	/**
	 * Gets the approval level label.
	 *
	 * @return the approvalLevelLabel
	 */
	public String getApprovalLevelLabel() {

		return approvalLevelLabel;
	}

	/**
	 * Sets the approval level label.
	 *
	 * @param approvalLevelLabel the approvalLevelLabel to set
	 */
	public void setApprovalLevelLabel(String approvalLevelLabel) {

		this.approvalLevelLabel = approvalLevelLabel;
	}

	/**
	 * Gets the approved by.
	 *
	 * @return the approvedBy
	 */
	public String getApprovedBy() {

		return approvedBy;
	}

	/**
	 * Sets the approved by.
	 *
	 * @param approvedBy the approvedBy to set
	 */
	public void setApprovedBy(String approvedBy) {

		this.approvedBy = approvedBy;
	}

}
