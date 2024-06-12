/*
 * 
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

/**
 * The Class ClaimsListDTO.
 */
public class ClaimsListDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 439087527961049655L;

	/** The id. */
	private Long id;

	/** The subject. */
	private String subject;

	/** The category. */
	private String category;

	/** The customer. */
	private String customer;

	/** The due date. */
	private Date dueDate;

	/** The date insertion. */
	private Date dateInsertion;

	/** The owner name. */
	private String ownerName;

	/** The estimation. */
	private Integer estimation;

	/** The priority. */
	private String priority;

	/** The claim owner. */
	private String claimOwner;

	/** The claim group owner. */
	private String claimGroupOwner;

	/** The claim. */
	private AcmClaimsDTO claim;

	/**
	 * Instantiates a new claims list DTO.
	 */
	public ClaimsListDTO() {

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
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the subject.
	 *
	 * @return the subject
	 */
	public String getSubject() {

		return subject;
	}

	/**
	 * Sets the subject.
	 *
	 * @param subject the new subject
	 */
	public void setSubject(String subject) {

		this.subject = subject;
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
	 * @param category the new category
	 */
	public void setCategory(String category) {

		this.category = category;
	}

	/**
	 * Gets the customer.
	 *
	 * @return the customer
	 */
	public String getCustomer() {

		return customer;
	}

	/**
	 * Sets the customer.
	 *
	 * @param customer the new customer
	 */
	public void setCustomer(String customer) {

		this.customer = customer;
	}

	/**
	 * Gets the due date.
	 *
	 * @return the due date
	 */
	public Date getDueDate() {

		return dueDate;
	}

	/**
	 * Sets the due date.
	 *
	 * @param dueDate the new due date
	 */
	public void setDueDate(Date dueDate) {

		this.dueDate = dueDate;
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
	 * Gets the estimation.
	 *
	 * @return the estimation
	 */
	public Integer getEstimation() {

		return estimation;
	}

	/**
	 * Sets the estimation.
	 *
	 * @param estimation the new estimation
	 */
	public void setEstimation(Integer estimation) {

		this.estimation = estimation;
	}

	/**
	 * Gets the priority.
	 *
	 * @return the priority
	 */
	public String getPriority() {

		return priority;
	}

	/**
	 * Sets the priority.
	 *
	 * @param priority the new priority
	 */
	public void setPriority(String priority) {

		this.priority = priority;
	}

	/**
	 * Gets the claim owner.
	 *
	 * @return the claim owner
	 */
	public String getClaimOwner() {

		return claimOwner;
	}

	/**
	 * Sets the claim owner.
	 *
	 * @param claimOwner the new claim owner
	 */
	public void setClaimOwner(String claimOwner) {

		this.claimOwner = claimOwner;
	}

	/**
	 * Gets the claim group owner.
	 *
	 * @return the claim group owner
	 */
	public String getClaimGroupOwner() {

		return claimGroupOwner;
	}

	/**
	 * Sets the claim group owner.
	 *
	 * @param claimGroupOwner the new claim group owner
	 */
	public void setClaimGroupOwner(String claimGroupOwner) {

		this.claimGroupOwner = claimGroupOwner;
	}

	/**
	 * Gets the claim.
	 *
	 * @return the claim
	 */
	public AcmClaimsDTO getClaim() {

		return claim;
	}

	/**
	 * Sets the claim.
	 *
	 * @param claim the new claim
	 */
	public void setClaim(AcmClaimsDTO claim) {

		this.claim = claim;
	}

}
