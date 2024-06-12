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
 * {@link LoanHistorique} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@Entity
@Table(name = "ACM_LOAN_HISTORIQUE")
public class LoanHistorique implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4254591194730341889L;

	/** The id loan historique. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_LOAN_HISTORIQUE", unique = true, nullable = false)
	private Long idLoanHistorique;

	/** The action. */
	@Column(name = "ACTION", nullable = false)
	private String action;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The date update. */
	@Column(name = "DATE_UPDATE")
	private Date dateUpdate;

	/** The updated by. */
	@Column(name = "UPDATED_BY", length = 256)
	private String updatedBy;

	/** The category. */
	@Column(name = "CATEGORY", length = 256)
	private String category;

	/** The techniqueInformation. */
	@Column(name = "TECHNIQUE_INFORMATION")
	private Boolean techniqueInformation;

	/** The loan. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_LOAN")
	private Loan loan;

	/**
	 * Instantiates a new loan historique.
	 */
	public LoanHistorique() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the id loan historique.
	 *
	 * @return the idLoanHistorique
	 */
	public Long getIdLoanHistorique() {

		return idLoanHistorique;
	}

	/**
	 * Sets the id loan historique.
	 *
	 * @param idLoanHistorique the idLoanHistorique to set
	 */
	public void setIdLoanHistorique(Long idLoanHistorique) {

		this.idLoanHistorique = idLoanHistorique;
	}

	/**
	 * Gets the action.
	 *
	 * @return the action
	 */
	public String getAction() {

		return action;
	}

	/**
	 * Sets the action.
	 *
	 * @param action the action to set
	 */
	public void setAction(String action) {

		this.action = action;
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
	 * @param description the description to set
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the date update.
	 *
	 * @return the dateUpdate
	 */
	public Date getDateUpdate() {

		return dateUpdate;
	}

	/**
	 * Sets the date update.
	 *
	 * @param dateUpdate the dateUpdate to set
	 */
	public void setDateUpdate(Date dateUpdate) {

		this.dateUpdate = dateUpdate;
	}

	/**
	 * Gets the updated by.
	 *
	 * @return the updatedBy
	 */
	public String getUpdatedBy() {

		return updatedBy;
	}

	/**
	 * Sets the updated by.
	 *
	 * @param updatedBy the updatedBy to set
	 */
	public void setUpdatedBy(String updatedBy) {

		this.updatedBy = updatedBy;
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
	 * @param category the category to set
	 */
	public void setCategory(String category) {

		this.category = category;
	}

	/**
	 * Gets the technique information.
	 *
	 * @return the techniqueInformation
	 */
	public Boolean getTechniqueInformation() {

		return techniqueInformation;
	}

	/**
	 * Sets the techniqueInformation.
	 * 
	 * @param techniqueInformation the techniqueInformation to set
	 */
	public void setTechniqueInformation(Boolean techniqueInformation) {

		this.techniqueInformation = techniqueInformation;
	}
}
