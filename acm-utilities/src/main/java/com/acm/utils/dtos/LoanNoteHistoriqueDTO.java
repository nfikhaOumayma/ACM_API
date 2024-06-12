/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * {@link LoanNoteHistoriqueDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.11.0
 */
public class LoanNoteHistoriqueDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8825928524215971883L;

	/** The loan DTO. */
	private LoanDTO loanDTO;

	/** The approval amount. */
	private Long approvalAmount;

	/** The approval desicion. */
	private Integer approvalDesicion;

	/** The approval desicion label. */
	private String approvalDesicionLabel;

	/** The approval level. */
	private Integer approvalLevel;

	/** The approval level label. */
	private String approvalLevelLabel;

	/** The approved by. */
	private String approvedBy;

	/** The comments. */
	private String comments;

	/** The status id. */
	private Integer statusId;

	/** The status libelle. */
	private String statusLibelle;

	/** The amount. */
	private BigDecimal amount;

	/** The insert by. */
	private String insertBy;

	/** The type note. NOTE / APPROVEL */
	private String typeNote;

	/** The action date. */
	private Date actionDate;

	/**
	 * Instantiates a new loan approval historique DTO.
	 */
	public LoanNoteHistoriqueDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new loan note historique DTO to match {@link LoanApprovalHistoriqueDTO}.
	 *
	 * @param loanDTO the loan DTO
	 * @param actionDate the action date
	 * @param approvalAmount the approval amount
	 * @param approvalDesicion the approval desicion
	 * @param approvalDesicionLabel the approval desicion label
	 * @param comments the comments
	 * @param approvalLevel the approval level
	 * @param approvalLevelLabel the approval level label
	 * @param approvedBy the approved by
	 * @param typeNote the type note
	 */
	public LoanNoteHistoriqueDTO(LoanDTO loanDTO, Date actionDate, Long approvalAmount,
			Integer approvalDesicion, String approvalDesicionLabel, String comments,
			Integer approvalLevel, String approvalLevelLabel, String approvedBy, String typeNote) {

		this.loanDTO = loanDTO;
		this.actionDate = actionDate;
		this.approvalAmount = approvalAmount;
		this.approvalDesicion = approvalDesicion;
		this.approvalDesicionLabel = approvalDesicionLabel;
		this.comments = comments;
		this.approvalLevel = approvalLevel;
		this.approvalLevelLabel = approvalLevelLabel;
		this.approvedBy = approvedBy;
		this.typeNote = typeNote;
	}

	/**
	 * Instantiates a new loan note historique DTO to match {@link CustomerDecisionDTO}.
	 *
	 * @param loanDTO the loan DTO
	 * @param actionDate the action date
	 * @param comments the comments
	 * @param statusId the status id
	 * @param statusLibelle the status libelle
	 * @param amount the amount
	 * @param insertBy the insert by
	 * @param typeNote the type note
	 */
	public LoanNoteHistoriqueDTO(LoanDTO loanDTO, Date actionDate, String comments,
			Integer statusId, String statusLibelle, BigDecimal amount, String insertBy,
			String typeNote) {

		this.loanDTO = loanDTO;
		this.actionDate = actionDate;
		this.comments = comments;
		this.statusId = statusId;
		this.statusLibelle = statusLibelle;
		this.amount = amount;
		this.insertBy = insertBy;
		this.typeNote = typeNote;
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

	/**
	 * Gets the comments.
	 *
	 * @return the comments
	 */
	public String getComments() {

		return comments;
	}

	/**
	 * Sets the comments.
	 *
	 * @param comments the comments to set
	 */
	public void setComments(String comments) {

		this.comments = comments;
	}

	/**
	 * Gets the status id.
	 *
	 * @return the statusId
	 */
	public Integer getStatusId() {

		return statusId;
	}

	/**
	 * Sets the status id.
	 *
	 * @param statusId the statusId to set
	 */
	public void setStatusId(Integer statusId) {

		this.statusId = statusId;
	}

	/**
	 * Gets the status libelle.
	 *
	 * @return the statusLibelle
	 */
	public String getStatusLibelle() {

		return statusLibelle;
	}

	/**
	 * Sets the status libelle.
	 *
	 * @param statusLibelle the statusLibelle to set
	 */
	public void setStatusLibelle(String statusLibelle) {

		this.statusLibelle = statusLibelle;
	}

	/**
	 * Gets the amount.
	 *
	 * @return the amount
	 */
	public BigDecimal getAmount() {

		return amount;
	}

	/**
	 * Sets the amount.
	 *
	 * @param amount the amount to set
	 */
	public void setAmount(BigDecimal amount) {

		this.amount = amount;
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
	 * Gets the type note.
	 *
	 * @return the typeNote
	 */
	public String getTypeNote() {

		return typeNote;
	}

	/**
	 * Sets the type note.
	 *
	 * @param typeNote the typeNote to set
	 */
	public void setTypeNote(String typeNote) {

		this.typeNote = typeNote;
	}

	/**
	 * Gets the action date.
	 *
	 * @return the actionDate
	 */
	public Date getActionDate() {

		return actionDate;
	}

	/**
	 * Sets the action date.
	 *
	 * @param actionDate the actionDate to set
	 */
	public void setActionDate(Date actionDate) {

		this.actionDate = actionDate;
	}

}
