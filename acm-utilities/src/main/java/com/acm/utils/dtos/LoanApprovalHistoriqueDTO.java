/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

import org.dozer.Mapping;

/**
 * {@link LoanApprovalHistoriqueDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.6.0
 */
public class LoanApprovalHistoriqueDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1065008644200257455L;

	/** The id. */
	private Long id;

	/** The loan DTO. */
	@Mapping("loan")
	private LoanDTO loanDTO;

	/** The approval date. */
	private Date approvalDate;

	/** The approval amount. */
	private Long approvalAmount;

	/** The approval desicion. */
	private Integer approvalDesicion;

	/** The approval desicion label. */
	private String approvalDesicionLabel;

	/** The approval note. */
	private String approvalNote;

	/** The approval level. */
	private Integer approvalLevel;

	/** The approval level label. */
	private String approvalLevelLabel;

	/** The approved by. */
	private String approvedBy;

	/**
	 * Instantiates a new loan approval historique DTO.
	 */
	public LoanApprovalHistoriqueDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new loan approval historique DTO.
	 *
	 * @param loanDTO the loan DTO
	 * @param approvalAmount the approval amount
	 * @param approvalDesicion the approval desicion
	 * @param approvalNote the approval note
	 * @param approvalLevel the approval level
	 */
	public LoanApprovalHistoriqueDTO(LoanDTO loanDTO, Long approvalAmount, Integer approvalDesicion,
			String approvalNote, Integer approvalLevel) {

		this.loanDTO = loanDTO;
		this.approvalAmount = approvalAmount;
		this.approvalDesicion = approvalDesicion;
		this.approvalNote = approvalNote;
		this.approvalLevel = approvalLevel;
	}

	/**
	 * Instantiates a new loan approval historique DTO.
	 *
	 * @param loanDTO the loan DTO
	 */
	public LoanApprovalHistoriqueDTO(LoanDTO loanDTO) {

		this.loanDTO = loanDTO;
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

}
