/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

import org.dozer.Mapping;

/**
 * {@link LoanHistoriqueDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
public class LoanHistoriqueDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -8600130295534911830L;

	/** The id document. */
	private Long idLoanHistorique;

	/** The titre. */
	private String action;

	/** The description. */
	private String description;

	/** The date last update. */
	private Date dateUpdate;

	/** The updated by. */
	private String updatedBy;

	/** The category. */
	private String category;

	/** The techniqueInformation. */
	private Boolean techniqueInformation;

	/** The loan. */
	@Mapping("loan")
	private LoanDTO loanDTO;

	/**
	 * Instantiates a new loan historique DTO.
	 */
	public LoanHistoriqueDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new loan historique DTO.
	 *
	 * @param action the action
	 * @param description the description
	 * @param updatedBy the updated by
	 * @param loanDTO the loan DTO
	 * @param category the category
	 * @param techniqueInformation the techniqueInformation
	 */
	public LoanHistoriqueDTO(String action, String description, String updatedBy, LoanDTO loanDTO,
			String category, boolean techniqueInformation) {

		this.action = action;
		this.description = description;
		this.loanDTO = loanDTO;
		this.updatedBy = updatedBy;
		this.category = category;
		this.techniqueInformation = techniqueInformation;
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
	 * Gets the loan DTO.
	 *
	 * @return the loanDTO
	 */
	public LoanDTO getLoanDTO() {

		return loanDTO;
	}

	/**
	 * Sets the loan DTO.
	 *
	 * @param loanDTO the loanDTO to set
	 */
	public void setLoanDTO(LoanDTO loanDTO) {

		this.loanDTO = loanDTO;
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
