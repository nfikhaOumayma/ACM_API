package com.acm.utils.models;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * The Class Item.
 */
@Entity
@Table(name = "ACM_ITEM")
public class Item extends GenericModel implements Serializable {
	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_ITEM", unique = true, nullable = false)
	private Long id;

	/** The generic work flow object. */
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "ID_ACM_GENERIC_WORKFLOW_OBJECT")
	private GenericWorkFlowObject genericWorkFlowObject;

	/** The branches. */
	@Column(name = "ACCESS_BRANCHE")
	private String branches;

	/** The status. */
	@Column(name = "STATUS")
	private Integer status;

	/** The actual step. */
	@Column(name = "ID_ACM_INSTANCE_ITEM_ACTUAL_STEP")
	private Long actualStepInstance;

	/** The actual step. */
	@Column(name = "ID_ACTUAL_STEP_WF")
	private Long actualStep;

	/** The item instances. */
	@OneToMany(mappedBy = "item")
	private Set<ItemInstance> itemInstances = new HashSet<>();

	/** The review from step. */
	@Column(name = "REVIEW_FROM_STEP")
	private Long reviewFromStep;

	/** The owner. */
	@Column(name = "OWNER")
	private String owner;

	/** The owner name. */
	@Column(name = "OWNER_NAME")
	private String ownerName;

	/** The owner email. */
	@Column(name = "OWNER_EMAIL")
	private String ownerEmail;

	/** The portfolio id. */
	@Column(name = "PORTFOLIO_ID")
	private Long portfolioId;

	/** The group owner name. */
	@Column(name = "GROUP_OWNER_NAME")
	private String groupOwnerName;

	/** The group owner. */
	@Column(name = "GROUP_OWNER")
	private String groupOwner;

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
	public GenericWorkFlowObject getGenericWorkFlowObject() {

		return genericWorkFlowObject;
	}

	/**
	 * Sets the generic work flow object.
	 *
	 * @param genericWorkFlowObject the new generic work flow object
	 */
	public void setGenericWorkFlowObject(GenericWorkFlowObject genericWorkFlowObject) {

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
	 * Gets the item instances.
	 *
	 * @return the item instances
	 */
	public Set<ItemInstance> getItemInstances() {

		return itemInstances;
	}

	/**
	 * Sets the item instances.
	 *
	 * @param itemInstances the new item instances
	 */
	public void setItemInstances(Set<ItemInstance> itemInstances) {

		this.itemInstances = itemInstances;
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

}
