/*
 * a * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium
 * is strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.dozer.Mapping;

import com.acm.constants.common.CommonFunctions;

// TODO: Auto-generated Javadoc
/**
 * {@link WorkFlowStepDTO} class.
 *
 * @author yesser somai
 * @since 1.0.10
 */

public class WorkFlowStepDTO extends GenericDTO
		implements Serializable, Comparable<WorkFlowStepDTO> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7848770534480354810L;

	/** The id work flow step. */
	private Long idWorkFlowStep;

	/** The step name. */
	private String stepName;

	/** The order. */
	private Long order;

	/** The step Type. */
	private String stepType;

	/** The product id. */
	private long productId;

	/** The amount. */
	private BigDecimal minAmount;

	/** The max amount. */
	private BigDecimal maxAmount;

	/** The user group. */
	private String userGroup;

	/** The previous step. */
	private String previousStep;

	/** The enabled. */
	private Boolean enabled;

	/** The process. */
	private String process;

	/** The screen. */
	private String screen;

	/** The code statut loan. */
	private Long codeStatutLoan;

	/** The code statut loan. */
	private Long processVersion;

	/** The group code. */
	private String groupCode;

	/** The participants loans. */
	@Mapping("participants")
	private List<GroupeDTO> participants;

	/** The participants approval loans. */
	@Mapping("approvers")
	private List<GroupeDTO> approvers;

	/** The ready for disb. */
	private Boolean readyForDisb;

	/** The acm doc prod collection. */
	@Mapping("documents")
	private List<SettingDocumentProductDTO> documents;

	/** The Workflow step udf groupe. */
	private List<WorkflowStepUdfGroupeDTO> workflowStepUdfGroupe;

	/** The journal entry types. */
	private List<SettingJournalEntryTypeDTO> journalEntryTypes =
			new ArrayList<SettingJournalEntryTypeDTO>();

	/** The charge fees. */
	private Set<SettingChargeFeeDTO> listChargeFees = new HashSet<>();

	/** The approval conditions. */
	private Boolean approvalConditions;

	/** The screening component. */
	private String screeningComponent;

	/** The generation task. */
	private Boolean generationTask;

	/** The ib screen. */
	private String ibScreen;
	/** The check meza card. */
	private Boolean checkMezaCard;
	/** The check fees. */
	private Boolean checkFees;

	/** The lst fees. */
	private Set<ApplicationFeeDTO> lstFees = new HashSet<>();

	/** The lst fees. */
	private Set<SettingListValuesDTO> lstFeesListValue = new HashSet<>();

	/** The automatic step. */
	private Boolean automaticStep;

	/** The min score rejected. */
	private Long minScoreRejected;

	/** The max score rejected. */
	private Long maxScoreRejected;

	/** The min score accepted. */
	private Long minScoreAccepted;

	/** The max score accepted. */
	private Long maxScoreAccepted;

	/** The rejection condition. */
	private String rejectionCondition;

	/** The acceptation condition. */
	private String acceptationCondition;
	/** The planing dto. */
	private PlaningStepDTO planingDto;

	/** The list risk setting. */
	private List<RiskSettingStepDTO> listRiskSetting;

	/** The active timer loan. */
	private Boolean activeTimerLoan;
	/** The udf groups. */
	private List<UserDefinedFieldGroupDTO> udfGroups = new ArrayList<>();
	/** The code acm template SMS. */
	private String codeAcmTemplateSms;

	/** The document types. */
	private List<SettingDocumentTypeDTO> documentTypes;

	/** The object id. */
	private Long objectId;

	/**
	 * Gets the acm template SMS.
	 *
	 * @return the acm template SMS
	 */
	public String getCodeAcmTemplateSms() {

		return codeAcmTemplateSms;
	}

	/**
	 * Sets the acm template SMS.
	 *
	 * @param codeAcmTemplateSms the new code acm template sms
	 */
	public void setCodeAcmTemplateSms(String codeAcmTemplateSms) {

		this.codeAcmTemplateSms = codeAcmTemplateSms;
	}

	/**
	 * Instantiates a new work flow step.
	 */
	public WorkFlowStepDTO() {

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

	public List<WorkflowStepUdfGroupeDTO> getWorkflowStepUdfGroupe() {

		return workflowStepUdfGroupe;
	}

	/**
	 * Sets the workflow step udf groupe.
	 *
	 * @param WorkflowStepUdfGroupe the new workflow step udf groupe
	 */
	public void setWorkflowStepUdfGroupe(List<WorkflowStepUdfGroupeDTO> WorkflowStepUdfGroupe) {

		workflowStepUdfGroupe = WorkflowStepUdfGroupe;
	}

	/**
	 * Gets the documents.
	 *
	 * @return the documents
	 */
	public List<SettingDocumentProductDTO> getDocuments() {

		return documents;
	}

	/**
	 * Sets the documents.
	 *
	 * @param documents the new documents
	 */
	public void setDocuments(List<SettingDocumentProductDTO> documents) {

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
	 * Gets the enabled.
	 *
	 * @return the enabled
	 */
	public Boolean getEnabled() {

		return enabled;
	}

	/**
	 * Sets the enabled.
	 *
	 * @param enabled the enabled to set
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
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
	 * Gets the code statut loan.
	 *
	 * @return the codeStatutLoan
	 */
	public Long getCodeStatutLoan() {

		return codeStatutLoan;
	}

	/**
	 * Sets the code statut loan.
	 *
	 * @param codeStatutLoan the codeStatutLoan to set
	 */
	public void setCodeStatutLoan(Long codeStatutLoan) {

		this.codeStatutLoan = codeStatutLoan;
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
	 * Compare to.
	 *
	 * @param o the o
	 * @return the int
	 */
	/*
	 * (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(WorkFlowStepDTO o) {

		if (this.process.equals(o.process)) {
			return this.order.compareTo(o.order);
		}
		else {
			return processIndex(this.process).compareTo(processIndex(o.process));
		}
	}

	/**
	 * Gets the process.
	 *
	 * @param process the process to set
	 * @return processIndex
	 */
	private Integer processIndex(String process) {

		switch (process) {
			case "DRAFT":
				return 0;
			case "APPROVAL":
				return 1;
			case "APPROVED":
				return 2;
		}
		return -1;
	}

	/**
	 * Gets the participants.
	 *
	 * @return the participants
	 */
	public List<GroupeDTO> getParticipants() {

		return participants;
	}

	/**
	 * Sets the participants.
	 *
	 * @param participants the new participants
	 */
	public void setParticipants(List<GroupeDTO> participants) {

		this.participants = participants;
	}

	/**
	 * Gets the approvers.
	 *
	 * @return the approvers
	 */
	public List<GroupeDTO> getApprovers() {

		return approvers;
	}

	/**
	 * Sets the approvers.
	 *
	 * @param approvers the new approvers
	 */
	public void setApprovers(List<GroupeDTO> approvers) {

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
	public List<SettingJournalEntryTypeDTO> getJournalEntryTypes() {

		return journalEntryTypes;
	}

	/**
	 * Sets the journal entry types.
	 *
	 * @param journalEntryTypes the journalEntryTypes to set
	 */
	public void setJournalEntryTypes(List<SettingJournalEntryTypeDTO> journalEntryTypes) {

		this.journalEntryTypes = journalEntryTypes;
	}

	/**
	 * <<<<<<< HEAD Gets the ib screen.
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
	 * Gets the lst fees.
	 *
	 * @return the lstFees
	 */
	public Set<ApplicationFeeDTO> getLstFees() {

		lstFeesListValue.forEach(item -> {
			lstFees.add((ApplicationFeeDTO) CommonFunctions
					.convertJSONStringtoObject(item.getValueJson(), ApplicationFeeDTO.class));
		});

		return lstFees;
	}

	/**
	 * Sets the lst fees.
	 *
	 * @param lstFees the lstFees to set
	 */
	public void setLstFees(Set<ApplicationFeeDTO> lstFees) {

		this.lstFees = lstFees;
	}

	/**
	 * Gets the lst fees list value.
	 *
	 * @return the lstFeesListValue
	 */
	public Set<SettingListValuesDTO> getLstFeesListValue() {

		return lstFeesListValue;
	}

	/**
	 * Sets the lst fees list value.
	 *
	 * @param lstFeesListValue the lstFeesListValue to set
	 */
	public void setLstFeesListValue(Set<SettingListValuesDTO> lstFeesListValue) {

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
	 * <<<<<<< HEAD <<<<<<< HEAD Gets the planing dto.
	 *
	 * @return the planing dto
	 */
	public PlaningStepDTO getPlaningDto() {

		return planingDto;
	}

	/**
	 * Sets the planing dto.
	 *
	 * @param planingDto the new planing dto
	 */
	public void setPlaningDto(PlaningStepDTO planingDto) {

		this.planingDto = planingDto;
	}

	/**
	 * Gets the list risk setting.
	 *
	 * @return the list risk setting
	 */
	public List<RiskSettingStepDTO> getListRiskSetting() {

		return listRiskSetting;
	}

	/**
	 * Sets the list risk setting.
	 *
	 * @param listRiskSetting the new list risk setting
	 */
	public void setListRiskSetting(List<RiskSettingStepDTO> listRiskSetting) {

		this.listRiskSetting = listRiskSetting;
	}

	/**
	 * Gets the document types.
	 *
	 * @return the document types
	 */
	public List<SettingDocumentTypeDTO> getDocumentTypes() {

		return documentTypes;
	}

	/**
	 * Sets the document types.
	 *
	 * @param documentTypes the new document types
	 */
	public void setDocumentTypes(List<SettingDocumentTypeDTO> documentTypes) {

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
	public Set<SettingChargeFeeDTO> getListChargeFees() {

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
	public void setListChargeFees(Set<SettingChargeFeeDTO> listChargeFees) {

		this.listChargeFees = listChargeFees;
	}

	/**
	 * Gets the active timer loan.
	 *
	 * @return the active timer loan
	 */
	public Boolean getActiveTimerLoan() {

		return activeTimerLoan;
	}

	/**
	 * Sets the active timer loan.
	 *
	 * @param activeTimerLoan the new active timer loan
	 */
	public void setActiveTimerLoan(Boolean activeTimerLoan) {

		this.activeTimerLoan = activeTimerLoan;
	}

	/**
	 * Gets the udf groups.
	 *
	 * @return the udf groups
	 */
	public List<UserDefinedFieldGroupDTO> getUdfGroups() {

		return udfGroups;
	}

	/**
	 * Sets the udf groups.
	 *
	 * @param udfGroups the new udf groups
	 */
	public void setUdfGroups(List<UserDefinedFieldGroupDTO> udfGroups) {

		this.udfGroups = udfGroups;
	}

}
