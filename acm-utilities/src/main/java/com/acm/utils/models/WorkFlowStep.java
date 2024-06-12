/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * {@link WorkFlowStep} class.
 *
 * @author yesser somai
 * @since 1.0.10
 */
@Entity
@Table(name = "ACM_WORKFLOW_STEP")
public class WorkFlowStep extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7127718119619815549L;

	/** The id work flow step. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_WORKFLOW_STEP", unique = true, nullable = false)
	private Long idWorkFlowStep;

	/** The step name. */
	@Column(name = "STEP_NAME")
	private String stepName;

	/** The order. */
	@Column(name = "STEP_ORDER")
	private Long order;

	/** The step Type. */
	@Column(name = "STEP_TYPE")
	private String stepType;

	/** The product id. */
	@Column(name = "PRODUCT_ID")
	private long productId;

	/** The amount. */
	@Column(name = "MIN_AMOUNT", columnDefinition = "default 0")
	private BigDecimal minAmount;

	/** The max amount. */
	@Column(name = "MAX_AMOUNT")
	private BigDecimal maxAmount;

	/** The user group. */
	@Column(name = "USER_GROUP")
	private String userGroup;

	/** The previous step. */
	@Column(name = "PREVIOUS_STEP", columnDefinition = "default 0")
	private String previousStep;

	/** The process. */
	@Column(name = "PROCESS")
	private String process;

	/** The screen. */
	@Column(name = "SCREEN")
	private String screen;

	/** The code statut loan. */
	@Column(name = "CODE_STATUT_LOAN")
	private Long codeStatutLoan;

	/** The code statut loan. */
	@Column(name = "PROCESS_VERSION")
	private Long processVersion;

	/** The group code. */
	@Column(name = "GROUP_CODE")
	private String groupCode;

	/** The participants loans. */
	@ManyToMany(fetch = FetchType.EAGER)
	@JoinTable(name = "ACM_WORKFLOW_STEP_ACM_GROUPE_PARTICIPANT",
			joinColumns = {@JoinColumn(name = "ACM_LOAN_STEP_ID")},
			inverseJoinColumns = {@JoinColumn(name = "ACM_GROUPE_ID")})
	private Set<Groupe> participants = new HashSet<>();

	/** The participants approval loans. */
	@ManyToMany(fetch = FetchType.EAGER)
	@JoinTable(name = "ACM_WORKFLOW_STEP_ACM_GROUPE_APPROVAL",
			joinColumns = {@JoinColumn(name = "ACM_LOAN_STEP_ID")},
			inverseJoinColumns = {@JoinColumn(name = "ACM_GROUPE_ID")})
	private Set<Groupe> approvers = new HashSet<>();

	/** The journal entry types. */
	@ManyToMany(fetch = FetchType.EAGER)
	@JoinTable(name = "ACM_SETTING_JOURNAL_ENTRY_TYPE_ACM_WORKFLOW_STEP",
			joinColumns = {@JoinColumn(name = "ID_ACM_WORKFLOW_STEP")},
			inverseJoinColumns = {@JoinColumn(name = "ID_ACM_SETTING_JOURNAL_ENTRY")})
	private List<SettingJournalEntryType> journalEntryTypes = new ArrayList<>();

	/** The ready for disb. */
	@Column(name = "READY_FOR_DISB")
	private Boolean readyForDisb;

	/** The documents. */
	@ManyToMany(fetch = FetchType.EAGER)
	@JoinTable(name = "ACM_WORKFLOW_STEP_ACM_DOCUMENT_TYPE_PRODUCT",
			joinColumns = {@JoinColumn(name = "ACM_WORKFLOW_STEP_ID")},
			inverseJoinColumns = {@JoinColumn(name = "ACM_DOCU_TYPE_PROD_ID")})
	private Set<SettingDocumentProduct> documents = new HashSet<>();

	/** The approval conditions. */
	@Column(name = "APPROVAL_CONDITIONS")
	private Boolean approvalConditions;

	/** The screening component. */
	@Column(name = "SCREENING_COMPONENT")
	private String screeningComponent;

	/** The generation task. */
	@Column(name = "GENERATION_TASK")
	private Boolean generationTask;

	/** The screen. */
	@Column(name = "IB_SCREEN")
	private String ibScreen;
	/** The check meza card. */
	@Column(name = "CHECK_MEZA_CARD")
	private Boolean checkMezaCard;

	/** The check meza card. */
	@Column(name = "CHECK_FEES")
	private Boolean checkFees;

	/** The documents. */
	@ManyToMany(fetch = FetchType.EAGER)
	@JoinTable(name = "ACM_WORKFLOW_STEP_SETTING_VALUE_FEES",
			joinColumns = {@JoinColumn(name = "ACM_WORKFLOW_STEP_ID")},
			inverseJoinColumns = {@JoinColumn(name = "ID_ACM_SETTING_LIST_VALUES")})
	private Set<SettingListValues> lstFeesListValue = new HashSet<>();

	/** The list charge fees. */
	@ManyToMany(fetch = FetchType.EAGER)
	@JoinTable(name = "ACM_WORKFLOW_STEP_SETTING_CHARGE_FEES",
			joinColumns = {@JoinColumn(name = "ID_ACM_WORKFLOW_STEP")},
			inverseJoinColumns = {@JoinColumn(name = "ID_ACM_SETTING_CHARGE_FEE")})
	private Set<SettingChargeFee> listChargeFees = new HashSet<>();

	/** The automatic step. */
	@Column(name = "AUTOMATIC_STEP")
	private Boolean automaticStep;

	/** The min score rejected. */
	@Column(name = "MIN_SCORE_REJECTED")
	private Long minScoreRejected;

	/** The max score rejected. */
	@Column(name = "MAX_SCORE_REJECTED")
	private Long maxScoreRejected;

	/** The min score accepted. */
	@Column(name = "MIN_SCORE_ACCEPTED")
	private Long minScoreAccepted;

	/** The max score accepted. */
	@Column(name = "MAX_SCORE_ACCEPTED")
	private Long maxScoreAccepted;

	/** The rejection condition. */
	@Column(name = "REJECTION_CONDITION")
	private String rejectionCondition;

	/** The acceptation condition. */
	@Column(name = "ACCEPTATION_CONDITION")
	private String acceptationCondition;

	/** The code acm template SMS. */
	@Column(name = "CODE_ACM_TEMPLATE_SMS")
	private String codeAcmTemplateSms;

	/** The planing step. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_PLANING")
	private PlaningStep planingStep;

	/** The document types. */
	@ManyToMany(fetch = FetchType.EAGER)
	@JoinTable(name = "ACM_WORKFLOW_STEP_ACM_DOCUMENT_TYPE",
			joinColumns = {@JoinColumn(name = "ACM_WORKFLOW_STEP_ID")},
			inverseJoinColumns = {@JoinColumn(name = "ID_ACM_SETTING_DOC_TYPE")})
	private Set<SettingDocumentType> documentTypes = new HashSet<>();

	/** The object id. */
	@Column(name = "OBJECT_ID")
	private Long objectId;

	/**
	 * Gets the code acm template sms.
	 *
	 * @return the code acm template sms
	 */
	public String getCodeAcmTemplateSms() {

		return codeAcmTemplateSms;
	}

	/**
	 * Sets the code acm template sms.
	 *
	 * @param codeAcmTemplateSms the new code acm template sms
	 */
	public void setCodeAcmTemplateSms(String codeAcmTemplateSms) {

		this.codeAcmTemplateSms = codeAcmTemplateSms;
	}

	/**
	 * Instantiates a new work flow step.
	 */
	public WorkFlowStep() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the id work flow step.
	 *
	 * @return the idWorkFlowStep
	 */
	public Long getIdWorkFlowStep() {

		return idWorkFlowStep;
	}

	/**
	 * Gets the documents.
	 *
	 * @return the documents
	 */
	public Set<SettingDocumentProduct> getDocuments() {

		return documents;
	}

	/**
	 * Sets the documents.
	 *
	 * @param documents the new documents
	 */
	public void setDocuments(Set<SettingDocumentProduct> documents) {

		this.documents = documents;
	}

	/**
	 * Sets the id work flow step.
	 *
	 * @param idWorkFlowStep the idWorkFlowStep to set
	 */
	public void setIdWorkFlowStep(Long idWorkFlowStep) {

		this.idWorkFlowStep = idWorkFlowStep;
	}

	/**
	 * Gets the step name.
	 *
	 * @return the stepName
	 */
	public String getStepName() {

		return stepName;
	}

	/**
	 * Sets the step name.
	 *
	 * @param stepName the stepName to set
	 */
	public void setStepName(String stepName) {

		this.stepName = stepName;
	}

	/**
	 * Gets the order.
	 *
	 * @return the order
	 */
	public Long getOrder() {

		return order;
	}

	/**
	 * Sets the order.
	 *
	 * @param order the order to set
	 */
	public void setOrder(Long order) {

		this.order = order;
	}

	/**
	 * Gets the step type.
	 *
	 * @return the stepType
	 */
	public String getStepType() {

		return stepType;
	}

	/**
	 * Sets the step type.
	 *
	 * @param stepType the stepType to set
	 */
	public void setStepType(String stepType) {

		this.stepType = stepType;
	}

	/**
	 * Gets the product id.
	 *
	 * @return the productId
	 */
	public long getProductId() {

		return productId;
	}

	/**
	 * Sets the product id.
	 *
	 * @param productId the productId to set
	 */
	public void setProductId(long productId) {

		this.productId = productId;
	}

	/**
	 * Gets the user group.
	 *
	 * @return the userGroup
	 */
	public String getUserGroup() {

		return userGroup;
	}

	/**
	 * Sets the user group.
	 *
	 * @param userGroup the userGroup to set
	 */
	public void setUserGroup(String userGroup) {

		this.userGroup = userGroup;
	}

	/**
	 * Gets the previous step.
	 *
	 * @return the previousStep
	 */
	public String getPreviousStep() {

		return previousStep;
	}

	/**
	 * Sets the previous step.
	 *
	 * @param previousStep the previousStep to set
	 */
	public void setPreviousStep(String previousStep) {

		this.previousStep = previousStep;
	}

	/**
	 * Gets the process.
	 *
	 * @return the process
	 */
	public String getProcess() {

		return process;
	}

	/**
	 * Sets the process.
	 *
	 * @param process the process to set
	 */
	public void setProcess(String process) {

		this.process = process;
	}

	/**
	 * Gets the screen.
	 *
	 * @return the screen
	 */
	public String getScreen() {

		return screen;
	}

	/**
	 * Sets the screen.
	 *
	 * @param screen the screen to set
	 */
	public void setScreen(String screen) {

		this.screen = screen;
	}

	/**
	 * Gets the code status loan.
	 *
	 * @return the codeStatusLoan
	 */
	public Long getCodeStatutLoan() {

		return codeStatutLoan;
	}

	/**
	 * Sets the code status loan.
	 *
	 * @param codeStatusLoan the codeStatusLoan to set
	 */
	public void setCodeStatutLoan(Long codeStatusLoan) {

		this.codeStatutLoan = codeStatusLoan;
	}

	/**
	 * Gets the process version.
	 *
	 * @return the processVersion
	 */
	public Long getProcessVersion() {

		return processVersion;
	}

	/**
	 * Sets the process version.
	 *
	 * @param processVersion the processVersion to set
	 */
	public void setProcessVersion(Long processVersion) {

		this.processVersion = processVersion;
	}

	/**
	 * Gets the group code.
	 *
	 * @return the groupCode
	 */
	public String getGroupCode() {

		return groupCode;
	}

	/**
	 * Sets the group code.
	 *
	 * @param groupCode the groupCode to set
	 */
	public void setGroupCode(String groupCode) {

		this.groupCode = groupCode;
	}

	/**
	 * Gets the participants.
	 *
	 * @return the participants
	 */
	public Set<Groupe> getParticipants() {

		return participants;
	}

	/**
	 * Sets the participants.
	 *
	 * @param participants the new participants
	 */
	public void setParticipants(Set<Groupe> participants) {

		this.participants = participants;
	}

	/**
	 * Gets the approvers.
	 *
	 * @return the approvers
	 */
	public Set<Groupe> getApprovers() {

		return approvers;
	}

	/**
	 * Sets the approvers.
	 *
	 * @param approvers the new approvers
	 */
	public void setApprovers(Set<Groupe> approvers) {

		this.approvers = approvers;
	}

	/**
	 * Gets the ready for disb.
	 *
	 * @return the ready for disb
	 */

	public Boolean getReadyForDisb() {

		return readyForDisb;
	}

	/**
	 * Sets the ready for disb.
	 *
	 * @param readyForDisb the new ready for disb
	 */
	public void setReadyForDisb(Boolean readyForDisb) {

		this.readyForDisb = readyForDisb;
	}

	/**
	 * Gets the approval conditions.
	 *
	 * @return the approvalConditions
	 */
	public Boolean getApprovalConditions() {

		return approvalConditions;
	}

	/**
	 * Sets the approval conditions.
	 *
	 * @param approvalConditions the approvalConditions to set
	 */
	public void setApprovalConditions(Boolean approvalConditions) {

		this.approvalConditions = approvalConditions;
	}

	/**
	 * Gets the screening component.
	 *
	 * @return the screening component
	 */
	public String getScreeningComponent() {

		return screeningComponent;
	}

	/**
	 * Sets the screening component.
	 *
	 * @param screeningComponent the new screening component
	 */
	public void setScreeningComponent(String screeningComponent) {

		this.screeningComponent = screeningComponent;
	}

	/**
	 * Gets the generation task.
	 *
	 * @return the generation task
	 */
	public Boolean getGenerationTask() {

		return generationTask;
	}

	/**
	 * Sets the generation task.
	 *
	 * @param generationTask the new generation task
	 */
	public void setGenerationTask(Boolean generationTask) {

		this.generationTask = generationTask;
	}

	/**
	 * Gets the min amount.
	 *
	 * @return the min amount
	 */
	public BigDecimal getMinAmount() {

		return minAmount;
	}

	/**
	 * Sets the min amount.
	 *
	 * @param minAmount the new min amount
	 */
	public void setMinAmount(BigDecimal minAmount) {

		this.minAmount = minAmount;
	}

	/**
	 * Gets the max amount.
	 *
	 * @return the max amount
	 */
	public BigDecimal getMaxAmount() {

		return maxAmount;
	}

	/**
	 * Sets the max amount.
	 *
	 * @param maxAmount the new max amount
	 */
	public void setMaxAmount(BigDecimal maxAmount) {

		this.maxAmount = maxAmount;
	}

	/**
	 * Gets the journal entry types.
	 *
	 * @return the journalEntryTypes
	 */
	public List<SettingJournalEntryType> getJournalEntryTypes() {

		return journalEntryTypes;
	}

	/**
	 * Sets the journal entry types.
	 *
	 * @param journalEntryTypes the journalEntryTypes to set
	 */
	public void setJournalEntryTypes(List<SettingJournalEntryType> journalEntryTypes) {

		this.journalEntryTypes = journalEntryTypes;
	}

	/**
	 * Gets the ib screen.
	 *
	 * @return the ib screen
	 */
	public String getIbScreen() {

		return ibScreen;
	}

	/**
	 * Sets the ib screen.
	 *
	 * @param ibScreen the new ib screen
	 */
	public void setIbScreen(String ibScreen) {

		this.ibScreen = ibScreen;
	}

	/**
	 * Gets the check meza card.
	 *
	 * @return the checkMezaCard
	 */
	public Boolean getCheckMezaCard() {

		return checkMezaCard;
	}

	/**
	 * Sets the check meza card.
	 *
	 * @param checkMezaCard the checkMezaCard to set
	 */
	public void setCheckMezaCard(Boolean checkMezaCard) {

		this.checkMezaCard = checkMezaCard;
	}

	/**
	 * Gets the check fees.
	 *
	 * @return the checkFees
	 */
	public Boolean getCheckFees() {

		return checkFees;
	}

	/**
	 * Sets the check fees.
	 *
	 * @param checkFees the checkFees to set
	 */
	public void setCheckFees(Boolean checkFees) {

		this.checkFees = checkFees;
	}

	/**
	 * Gets the lst fees list value.
	 *
	 * @return the lstFeesListValue
	 */
	public Set<SettingListValues> getLstFeesListValue() {

		return lstFeesListValue;
	}

	/**
	 * Sets the lst fees list value.
	 *
	 * @param lstFeesListValue the lstFeesListValue to set
	 */
	public void setLstFeesListValue(Set<SettingListValues> lstFeesListValue) {

		this.lstFeesListValue = lstFeesListValue;
	}

	/**
	 * Gets the automatic step.
	 *
	 * @return the automatic step
	 */
	public Boolean getAutomaticStep() {

		return automaticStep;
	}

	/**
	 * Sets the automatic step.
	 *
	 * @param automaticStep the new automatic step
	 */
	public void setAutomaticStep(Boolean automaticStep) {

		this.automaticStep = automaticStep;
	}

	/**
	 * Gets the min score rejected.
	 *
	 * @return the min score rejected
	 */
	public Long getMinScoreRejected() {

		return minScoreRejected;
	}

	/**
	 * Sets the min score rejected.
	 *
	 * @param minScoreRejected the new min score rejected
	 */
	public void setMinScoreRejected(Long minScoreRejected) {

		this.minScoreRejected = minScoreRejected;
	}

	/**
	 * Gets the max score rejected.
	 *
	 * @return the max score rejected
	 */
	public Long getMaxScoreRejected() {

		return maxScoreRejected;
	}

	/**
	 * Sets the max score rejected.
	 *
	 * @param maxScoreRejected the new max score rejected
	 */
	public void setMaxScoreRejected(Long maxScoreRejected) {

		this.maxScoreRejected = maxScoreRejected;
	}

	/**
	 * Gets the min score accepted.
	 *
	 * @return the min score accepted
	 */
	public Long getMinScoreAccepted() {

		return minScoreAccepted;
	}

	/**
	 * Sets the min score accepted.
	 *
	 * @param minScoreAccepted the new min score accepted
	 */
	public void setMinScoreAccepted(Long minScoreAccepted) {

		this.minScoreAccepted = minScoreAccepted;
	}

	/**
	 * Gets the max score accepted.
	 *
	 * @return the max score accepted
	 */
	public Long getMaxScoreAccepted() {

		return maxScoreAccepted;
	}

	/**
	 * Sets the max score accepted.
	 *
	 * @param maxScoreAccepted the new max score accepted
	 */
	public void setMaxScoreAccepted(Long maxScoreAccepted) {

		this.maxScoreAccepted = maxScoreAccepted;
	}

	/**
	 * Gets the rejection condition.
	 *
	 * @return the rejection condition
	 */
	public String getRejectionCondition() {

		return rejectionCondition;
	}

	/**
	 * Sets the rejection condition.
	 *
	 * @param rejectionCondition the new rejection condition
	 */
	public void setRejectionCondition(String rejectionCondition) {

		this.rejectionCondition = rejectionCondition;
	}

	/**
	 * Gets the acceptation condition.
	 *
	 * @return the acceptation condition
	 */
	public String getAcceptationCondition() {

		return acceptationCondition;
	}

	/**
	 * Sets the acceptation condition.
	 *
	 * @param acceptationCondition the new acceptation condition
	 */
	public void setAcceptationCondition(String acceptationCondition) {

		this.acceptationCondition = acceptationCondition;
	}

	/**
	 * <<<<<<< HEAD Gets the planing step.
	 *
	 * @return the planing step
	 */
	public PlaningStep getPlaningStep() {

		return planingStep;
	}

	/**
	 * Sets the planing step.
	 *
	 * @param planingStep the new planing step
	 */
	public void setPlaningStep(PlaningStep planingStep) {

		this.planingStep = planingStep;
	}

	/**
	 * Gets the document types.
	 *
	 * @return the document types
	 */
	public Set<SettingDocumentType> getDocumentTypes() {

		return documentTypes;
	}

	/**
	 * Sets the document types.
	 *
	 * @param documentTypes the new document types
	 */
	public void setDocumentTypes(Set<SettingDocumentType> documentTypes) {

		this.documentTypes = documentTypes;
	}

	/**
	 * Gets the object id.
	 *
	 * @return the object id
	 */
	public Long getObjectId() {

		return objectId;
	}

	/**
	 * Gets the list charge fees.
	 *
	 * @return the list charge fees
	 */
	public Set<SettingChargeFee> getListChargeFees() {

		return listChargeFees;
	}

	/**
	 * Sets the object id.
	 *
	 * @param objectId the new object id
	 */
	public void setObjectId(Long objectId) {

		this.objectId = objectId;
	}

	/**
	 * Sets the list charge fees.
	 *
	 * @param listChargeFees the new list charge fees
	 */
	public void setListChargeFees(Set<SettingChargeFee> listChargeFees) {

		this.listChargeFees = listChargeFees;
	}

}
