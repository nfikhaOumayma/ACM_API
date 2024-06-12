/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.HashSet;
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
import javax.persistence.Table;

/**
 * {@link CollectionStep} class.
 *
 * @author Maher Khemissi
 * @since 1.0.10
 */
@Entity
@Table(name = "ACM_COLLECTION_STEP")
public class CollectionStep extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7127718119619815549L;

	/** The id collection step. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_COLLECTION_STEP", unique = true, nullable = false)
	private Long idCollectionStep;

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
	@Column(name = "AMOUNT", columnDefinition = "default 0")
	private BigDecimal amount;

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
	@Column(name = "PROCESS_VERSION")
	private Long processVersion;

	/** The start date. */
	@Column(name = "START_DATE")
	private int startDate;

	/** After. */
	@Column(name = "AFTER_DATE")
	private String afterDate;

	/** The unpaidAmount. */
	@Column(name = "UNPAID_AMOUNT", columnDefinition = "default 0")
	private BigDecimal unpaidAmount;

	/** The late date. */
	@Column(name = "LATE_DATE")
	private int lateDate;

	/** Reminder. */
	@Column(name = "REMINDER")
	private int reminder;

	/** Reminder Sup. */
	@Column(name = "REMINDER_SUP")
	private int reminderSup;

	/** The code statut loan. */
	@Column(name = "CODE_STATUT_LOAN")
	private String codeStatutLoan;

	/** The participants. */
	@ManyToMany(fetch = FetchType.EAGER)
	@JoinTable(name = "ACM_COLLECTION_STEP_ACM_GROUPE_PARTICIPANT",
			joinColumns = {@JoinColumn(name = "ACM_COLLECTION_STEP_ID")},
			inverseJoinColumns = {@JoinColumn(name = "ACM_GROUPE_ID")})
	private Set<Groupe> participants = new HashSet<>();

	/** The documents. */
	@ManyToMany(fetch = FetchType.EAGER)
	@JoinTable(name = "ACM_COLLECTION_STEP_ACM_DOCUMENT_TYPE_PRODUCT",
			joinColumns = {@JoinColumn(name = "ACM_COLLECTION_STEP_ID")},
			inverseJoinColumns = {@JoinColumn(name = "ACM_DOCU_TYPE_PROD_ID")})
	private Set<SettingDocumentProduct> documents = new HashSet<>();

	/** The list charge fees. */
	@ManyToMany(fetch = FetchType.EAGER)
	@JoinTable(name = "ACM_COLLECTION_STEP_SETTING_CHARGE_FEES",
			joinColumns = {@JoinColumn(name = "ID_ACM_COLLECTION_STEP")},
			inverseJoinColumns = {@JoinColumn(name = "ID_ACM_SETTING_CHARGE_FEE")})
	private Set<SettingChargeFee> listChargeFees = new HashSet<>();

	/** The type participants. */
	@Column(name = "TYPE_THIRD_PARTY")
	private String typeThirdParty;

	/** The group code. */
	@Column(name = "GROUP_CODE")
	private String groupCode;

	/** The generation task. */
	@Column(name = "GENERATION_TASK")
	private Boolean generationTask;

	/** The step tab. */
	@Column(name = "STEP_TAB")
	private String step_tab;

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
	@Column(name = "CODE_ACM_TEMPLATE_SMS")
	private String codeAcmTemplateSms;

	/**
	 * Gets the startDate.
	 *
	 * @return the startDate
	 */
	public int getStartDate() {

		return startDate;
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
	 * Sets the startDate.
	 *
	 * @param startDate the startDate to set
	 */
	public void setStartDate(int startDate) {

		this.startDate = startDate;
	}

	/**
	 * Gets the afterDate.
	 *
	 * @return the afterDate
	 */
	public String getAfterDate() {

		return afterDate;
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
	 * Sets the setAfterDate.
	 *
	 * @param afterDate the afterDate to set
	 */

	public void setAfterDate(String afterDate) {

		this.afterDate = afterDate;
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
	public int getLateDate() {

		return lateDate;
	}

	/**
	 * Sets the lateDate.
	 *
	 * @param lateDate the lateDate to set
	 */
	public void setLateDate(int lateDate) {

		this.lateDate = lateDate;
	}

	/**
	 * Gets the reminder.
	 *
	 * @return the reminder
	 */
	public int getReminder() {

		return reminder;
	}

	/**
	 * Sets the reminder.
	 *
	 * @param reminder the reminder to set
	 */
	public void setReminder(int reminder) {

		this.reminder = reminder;
	}

	/**
	 * Gets the reminderSup.
	 *
	 * @return the reminderSup
	 */
	public int getReminderSup() {

		return reminderSup;
	}

	/**
	 * Sets the reminderSup.
	 *
	 * @param reminderSup the reminderSup to set
	 */
	public void setReminderSup(int reminderSup) {

		this.reminderSup = reminderSup;
	}

	/**
	 * Instantiates a new work flow step.
	 */
	public CollectionStep() {

		/*
		 * EMPTY
		 */
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
	 * Gets the list charge fees.
	 *
	 * @return the list charge fees
	 */
	public Set<SettingChargeFee> getListChargeFees() {

		return listChargeFees;
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
