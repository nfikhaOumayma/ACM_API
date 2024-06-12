/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.dozer.Mapping;

// TODO: Auto-generated Javadoc
/**
 * {@link CollectionStepDTO} class.
 *
 * @author Maher Khemissi
 * @since 1.0.10
 */

public class CollectionStepDTO extends GenericDTO
		implements Serializable, Comparable<CollectionStepDTO> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7848770534480354810L;

	/** The id collection step. */
	private Long idCollectionStep;

	/** The step name. */
	private String stepName;

	/** The order. */
	private Long order;

	/** The step Type. */
	private String stepType;

	/** The product id. */
	private Long productId;

	/** The amount. */
	private BigDecimal amount;

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
	private Long processVersion;

	/** The start date. */
	private Integer startDate;

	/** afterDate. */
	private String afterDate;

	/** The unpaidAmount. */
	private BigDecimal unpaidAmount;

	/** The late date. */
	private Integer lateDate;

	/** Reminder. */
	private Integer reminder;

	/** Reminder Sup. */
	private Integer reminderSup;

	/** The groups users collection. */
	@Mapping("participants")
	private List<GroupeDTO> participants;

	/** The documents. */
	@Mapping("documents")
	private List<SettingDocumentProductDTO> documents;

	/** The group code. */
	private String groupCode;

	/** The type third party. */
	private String typeThirdParty;

	/** The code statut loan. */
	private String codeStatutLoan;

	/** The generation task. */
	private Boolean generationTask;

	/** The step tab. */
	private String step_tab;

	/** The workflow step udf groupe. */
	private List<WorkflowStepUdfGroupeDTO> workflowStepUdfGroupe;

	/** The list charge fees. */
	private Set<SettingChargeFeeDTO> listChargeFees = new HashSet<>();

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

	/** The code acm template SMS. */
	private String codeAcmTemplateSms;

	/**
	 * Gets the workflow step udf groupe.
	 *
	 * @return the workflow step udf groupe
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
	 * Gets the code statut loan.
	 *
	 * @return the code statut loan
	 */
	public String getCodeStatutLoan() {

		return codeStatutLoan;
	}

	/**
	 * Gets the step tab.
	 *
	 * @return the step tab
	 */
	public String getStep_tab() {

		return step_tab;
	}

	/**
	 * Sets the step tab.
	 *
	 * @param step_tab the new step tab
	 */
	public void setStep_tab(String step_tab) {

		this.step_tab = step_tab;
	}

	/**
	 * Sets the code statut loan.
	 *
	 * @param codeStatutLoan the new code statut loan
	 */
	public void setCodeStatutLoan(String codeStatutLoan) {

		this.codeStatutLoan = codeStatutLoan;
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
	 * Instantiates a new work flow step.
	 */
	public CollectionStepDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the startDate.
	 *
	 * @return the startDate
	 */
	public Integer getStartDate() {

		return startDate;
	}

	/**
	 * Sets the startDate.
	 *
	 * @param startDate the startDate to set
	 */
	public void setStartDate(Integer startDate) {

		this.startDate = startDate;
	}

	/**
	 * Gets the unpaidAmount.
	 *
	 * @return the unpaidAmount
	 */
	public BigDecimal getUnpaidAmount() {

		return unpaidAmount;
	}

	/**
	 * Sets the unpaidAmount.
	 *
	 * @param unpaidAmount the unpaidAmount to set
	 */
	public void setUnpaidAmount(BigDecimal unpaidAmount) {

		this.unpaidAmount = unpaidAmount;
	}

	/**
	 * Gets the lateDate.
	 *
	 * @return the lateDate
	 */
	public Integer getLateDate() {

		return lateDate;
	}

	/**
	 * Sets the lateDate.
	 *
	 * @param lateDate the lateDate to set
	 */
	public void setLateDate(Integer lateDate) {

		this.lateDate = lateDate;
	}

	/**
	 * Gets the reminder.
	 *
	 * @return the reminder
	 */
	public Integer getReminder() {

		return reminder;
	}

	/**
	 * Sets the reminder.
	 *
	 * @param reminder the reminder to set
	 */
	public void setReminder(Integer reminder) {

		this.reminder = reminder;
	}

	/**
	 * Gets the reminderSup.
	 *
	 * @return the reminderSup
	 */
	public Integer getReminderSup() {

		return reminderSup;
	}

	/**
	 * Sets the reminderSup.
	 *
	 * @param reminderSup the reminderSup to set
	 */
	public void setReminderSup(Integer reminderSup) {

		this.reminderSup = reminderSup;
	}

	/**
	 * Gets the id collection step.
	 *
	 * @return the idCollectionStep
	 */
	public Long getIdCollectionStep() {

		return idCollectionStep;
	}

	/**
	 * Sets the id collection step.
	 *
	 * @param idCollectionStep the idCollectionStep to set
	 */
	public void setIdCollectionStep(Long idCollectionStep) {

		this.idCollectionStep = idCollectionStep;
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
	public Long getProductId() {

		return productId;
	}

	/**
	 * Sets the product id.
	 *
	 * @param productId the productId to set
	 */
	public void setProductId(Long productId) {

		this.productId = productId;
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
	 * @param amount the new amount
	 */
	public void setAmount(BigDecimal amount) {

		this.amount = amount;
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
	public int compareTo(CollectionStepDTO o) {

		return this.order.compareTo(o.order);

	}

	/**
	 * Gets the after date.
	 *
	 * @return the after date
	 */
	public String getAfterDate() {

		return afterDate;
	}

	/**
	 * Sets the after date.
	 *
	 * @param afterDate the new after date
	 */
	public void setAfterDate(String afterDate) {

		this.afterDate = afterDate;
	}

	/**
	 * Gets the group code.
	 *
	 * @return the group code
	 */
	public String getGroupCode() {

		return groupCode;
	}

	/**
	 * Sets the group code.
	 *
	 * @param groupCode the new group code
	 */
	public void setGroupCode(String groupCode) {

		this.groupCode = groupCode;
	}

	/**
	 * Gets the type third party.
	 *
	 * @return the type third party
	 */
	public String getTypeThirdParty() {

		return typeThirdParty;
	}

	/**
	 * Sets the type third party.
	 *
	 * @param typeThirdParty the new type third party
	 */
	public void setTypeThirdParty(String typeThirdParty) {

		this.typeThirdParty = typeThirdParty;
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
	 * Gets the list charge fees.
	 *
	 * @return the list charge fees
	 */
	public Set<SettingChargeFeeDTO> getListChargeFees() {

		return listChargeFees;
	}

	/**
	 * Sets the list charge fees.
	 *
	 * @param listChargeFees the new list charge fees
	 */
	public void setListChargeFees(Set<SettingChargeFeeDTO> listChargeFees) {

		this.listChargeFees = listChargeFees;
	}

}
