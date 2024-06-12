/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.dozer.Mapping;

/**
 * The Class ItemDTO.
 */
public class ItemDTO implements Serializable {

	/** The id. */

	private Long id;

	/** The generic work flow object. */

	private GenericWorkFlowObjectDTO genericWorkFlowObject;

	/** The branches. */

	private String branches;

	/** The status. */
	private Integer status;

	/** The enabled. */
	private Boolean enabled;

	/** The date insertion. */
	private Date dateInsertion;

	/** The date last update. */
	private Date dateLastUpdate;

	/** The updated by. */
	private String updatedBy;

	/** The insert by. */
	private String insertBy;

	/** The Item instance DT os. */
	@Mapping("itemInstances")
	private List<ItemInstanceDTO> itemInstanceDTOs = new ArrayList<>();

	/** The actual step. */
	private Long actualStep;

	/** The actual step instance. */
	private Long actualStepInstance;

	/** The review from step. */
	private Long reviewFromStep;
	/** The review only selected step. */
	private Boolean reviewOnlySelectedStep;

	/** The reason label. */
	private String reasonLabel;

	/** The status label. */
	private String statusLabel;

	/** The owner. */
	private String owner;

	/** The owner name. */
	private String ownerName;

	/** The owner email. */
	private String ownerEmail;

	/** The portfolio id. */
	private Long portfolioId;

	/** The group owner name. */
	private String groupOwnerName;

	/** The group owner. */
	private String groupOwner;

	/** The unassigned item status. */
	private Integer unassignedItemStatus;

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
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the generic work flow object.
	 *
	 * @return the generic work flow object
	 */
	public GenericWorkFlowObjectDTO getGenericWorkFlowObject() {

		return genericWorkFlowObject;
	}

	/**
	 * Sets the generic work flow object.
	 *
	 * @param genericWorkFlowObject the new generic work flow object
	 */
	public void setGenericWorkFlowObject(GenericWorkFlowObjectDTO genericWorkFlowObject) {

		this.genericWorkFlowObject = genericWorkFlowObject;
	}

	/**
	 * Gets the branches.
	 *
	 * @return the branches
	 */
	public String getBranches() {

		return branches;
	}

	/**
	 * Gets the status.
	 *
	 * @return the status
	 */
	public Integer getStatus() {

		return status;
	}

	/**
	 * Sets the status.
	 *
	 * @param status the new status
	 */
	public void setStatus(Integer status) {

		this.status = status;
	}

	/**
	 * Sets the branches.
	 *
	 * @param branches the new branches
	 */
	public void setBranches(String branches) {

		this.branches = branches;
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
	 * @param enabled the new enabled
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the date insertion.
	 *
	 * @return the date insertion
	 */
	public Date getDateInsertion() {

		return dateInsertion;
	}

	/**
	 * Sets the date insertion.
	 *
	 * @param dateInsertion the new date insertion
	 */
	public void setDateInsertion(Date dateInsertion) {

		this.dateInsertion = dateInsertion;
	}

	/**
	 * Gets the date last update.
	 *
	 * @return the date last update
	 */
	public Date getDateLastUpdate() {

		return dateLastUpdate;
	}

	/**
	 * Sets the date last update.
	 *
	 * @param dateLastUpdate the new date last update
	 */
	public void setDateLastUpdate(Date dateLastUpdate) {

		this.dateLastUpdate = dateLastUpdate;
	}

	/**
	 * Gets the updated by.
	 *
	 * @return the updated by
	 */
	public String getUpdatedBy() {

		return updatedBy;
	}

	/**
	 * Sets the updated by.
	 *
	 * @param updatedBy the new updated by
	 */
	public void setUpdatedBy(String updatedBy) {

		this.updatedBy = updatedBy;
	}

	/**
	 * Gets the insert by.
	 *
	 * @return the insert by
	 */
	public String getInsertBy() {

		return insertBy;
	}

	/**
	 * Sets the insert by.
	 *
	 * @param insertBy the new insert by
	 */
	public void setInsertBy(String insertBy) {

		this.insertBy = insertBy;
	}

	/**
	 * Gets the item instance DT os.
	 *
	 * @return the item instance DT os
	 */
	public List<ItemInstanceDTO> getItemInstanceDTOs() {

		return itemInstanceDTOs;
	}

	/**
	 * Sets the item instance DT os.
	 *
	 * @param itemInstanceDTOs the new item instance DT os
	 */
	public void setItemInstanceDTOs(List<ItemInstanceDTO> itemInstanceDTOs) {

		this.itemInstanceDTOs = itemInstanceDTOs;
	}

	/**
	 * Gets the actual step.
	 *
	 * @return the actual step
	 */
	public Long getActualStep() {

		return actualStep;
	}

	/**
	 * Sets the actual step.
	 *
	 * @param actualStep the new actual step
	 */
	public void setActualStep(Long actualStep) {

		this.actualStep = actualStep;
	}

	/**
	 * Gets the actual step instance.
	 *
	 * @return the actual step instance
	 */
	public Long getActualStepInstance() {

		return actualStepInstance;
	}

	/**
	 * Sets the actual step instance.
	 *
	 * @param actualStepInstance the new actual step instance
	 */
	public void setActualStepInstance(Long actualStepInstance) {

		this.actualStepInstance = actualStepInstance;
	}

	/**
	 * Gets the review from step.
	 *
	 * @return the review from step
	 */
	public Long getReviewFromStep() {

		return reviewFromStep;
	}

	/**
	 * Sets the review from step.
	 *
	 * @param reviewFromStep the new review from step
	 */
	public void setReviewFromStep(Long reviewFromStep) {

		this.reviewFromStep = reviewFromStep;
	}

	/**
	 * Gets the review only selected step.
	 *
	 * @return the review only selected step
	 */
	public Boolean getReviewOnlySelectedStep() {

		return reviewOnlySelectedStep;
	}

	/**
	 * Sets the review only selected step.
	 *
	 * @param reviewOnlySelectedStep the new review only selected step
	 */
	public void setReviewOnlySelectedStep(Boolean reviewOnlySelectedStep) {

		this.reviewOnlySelectedStep = reviewOnlySelectedStep;
	}

	/**
	 * Gets the reason label.
	 *
	 * @return the reason label
	 */
	public String getReasonLabel() {

		return reasonLabel;
	}

	/**
	 * Sets the reason label.
	 *
	 * @param reasonLabel the new reason label
	 */
	public void setReasonLabel(String reasonLabel) {

		this.reasonLabel = reasonLabel;
	}

	/**
	 * Gets the status label.
	 *
	 * @return the status label
	 */
	public String getStatusLabel() {

		return statusLabel;
	}

	/**
	 * Sets the status label.
	 *
	 * @param statusLabel the new status label
	 */
	public void setStatusLabel(String statusLabel) {

		this.statusLabel = statusLabel;
	}

	/**
	 * Gets the owner.
	 *
	 * @return the owner
	 */
	public String getOwner() {

		return owner;
	}

	/**
	 * Sets the owner.
	 *
	 * @param owner the new owner
	 */
	public void setOwner(String owner) {

		this.owner = owner;
	}

	/**
	 * Gets the owner name.
	 *
	 * @return the owner name
	 */
	public String getOwnerName() {

		return ownerName;
	}

	/**
	 * Sets the owner name.
	 *
	 * @param ownerName the new owner name
	 */
	public void setOwnerName(String ownerName) {

		this.ownerName = ownerName;
	}

	/**
	 * Gets the owner email.
	 *
	 * @return the owner email
	 */
	public String getOwnerEmail() {

		return ownerEmail;
	}

	/**
	 * Sets the owner email.
	 *
	 * @param ownerEmail the new owner email
	 */
	public void setOwnerEmail(String ownerEmail) {

		this.ownerEmail = ownerEmail;
	}

	/**
	 * Gets the portfolio id.
	 *
	 * @return the portfolio id
	 */
	public Long getPortfolioId() {

		return portfolioId;
	}

	/**
	 * Sets the portfolio id.
	 *
	 * @param portfolioId the new portfolio id
	 */
	public void setPortfolioId(Long portfolioId) {

		this.portfolioId = portfolioId;
	}

	/**
	 * Gets the group owner name.
	 *
	 * @return the group owner name
	 */
	public String getGroupOwnerName() {

		return groupOwnerName;
	}

	/**
	 * Sets the group owner name.
	 *
	 * @param groupOwnerName the new group owner name
	 */
	public void setGroupOwnerName(String groupOwnerName) {

		this.groupOwnerName = groupOwnerName;
	}

	/**
	 * Gets the group owner.
	 *
	 * @return the group owner
	 */
	public String getGroupOwner() {

		return groupOwner;
	}

	/**
	 * Sets the group owner.
	 *
	 * @param groupOwner the new group owner
	 */
	public void setGroupOwner(String groupOwner) {

		this.groupOwner = groupOwner;
	}

	/**
	 * Gets the unassigned item status.
	 *
	 * @return the unassigned item status
	 */
	public Integer getUnassignedItemStatus() {

		return unassignedItemStatus;
	}

	/**
	 * Sets the unassigned item status.
	 *
	 * @param unassignedItemStatus the new unassigned item status
	 */
	public void setUnassignedItemStatus(Integer unassignedItemStatus) {

		this.unassignedItemStatus = unassignedItemStatus;
	}

}
