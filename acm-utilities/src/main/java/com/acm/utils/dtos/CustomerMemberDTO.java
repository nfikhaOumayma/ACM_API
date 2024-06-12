/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

/**
 * {@link CustomerMemberDTO} class Used to load members for customer (GRP / ORG).
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class CustomerMemberDTO extends GenericDTO implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6688106513382312623L;

	/** The community id (GRP). */
	private Long communityId;

	/** The organisation id. */
	private Long organisationId;

	/** The relationship id. */
	private Long relationshipId;

	/** The customer id. */
	private Long customerId;

	/** The customer type. */
	private String customerRole;

	/** The id extern. */
	private Long idExtern;

	/**
	 * Instantiates a new customer member DTO.
	 */
	public CustomerMemberDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new customer member DTO (Used to load members for customer (GRP / ORG)).
	 *
	 * @param communityId the community id
	 * @param organisationId the organisation id
	 * @param customerId the customer id
	 * @param customerRole the customer role
	 */
	public CustomerMemberDTO(Long communityId, Long organisationId, Long customerId,
			String customerRole) {

		this.communityId = communityId;
		this.organisationId = organisationId;
		this.customerId = customerId;
		this.customerRole = customerRole;
	}

	/**
	 * Instantiates a new customer member DTO (Used to load relationships for customer.
	 *
	 * @param relationshipId the relationship id
	 * @param customerId the customer id
	 * @param customerRole the customer role
	 * @param idExtern the id extern
	 */
	public CustomerMemberDTO(Long relationshipId, Long customerId, String customerRole,
			Long idExtern) {

		this.relationshipId = relationshipId;
		this.customerId = customerId;
		this.customerRole = customerRole;
		this.idExtern = idExtern;
	}

	/**
	 * Gets the community id.
	 *
	 * @return the communityId
	 */
	public Long getCommunityId() {

		return communityId;
	}

	/**
	 * Sets the community id.
	 *
	 * @param communityId the communityId to set
	 */
	public void setCommunityId(Long communityId) {

		this.communityId = communityId;
	}

	/**
	 * Gets the organisation id.
	 *
	 * @return the organisationId
	 */
	public Long getOrganisationId() {

		return organisationId;
	}

	/**
	 * Sets the organisation id.
	 *
	 * @param organisationId the organisationId to set
	 */
	public void setOrganisationId(Long organisationId) {

		this.organisationId = organisationId;
	}

	/**
	 * Gets the customer id.
	 *
	 * @return the customerId
	 */
	public Long getCustomerId() {

		return customerId;
	}

	/**
	 * Sets the customer id.
	 *
	 * @param customerId the customerId to set
	 */
	public void setCustomerId(Long customerId) {

		this.customerId = customerId;
	}

	/**
	 * Gets the customer role.
	 *
	 * @return the customerRole
	 */
	public String getCustomerRole() {

		return customerRole;
	}

	/**
	 * Sets the customer role.
	 *
	 * @param customerRole the customerRole to set
	 */
	public void setCustomerRole(String customerRole) {

		this.customerRole = customerRole;
	}

	/**
	 * Gets the relationship id.
	 *
	 * @return the relationshipId
	 */
	public Long getRelationshipId() {

		return relationshipId;
	}

	/**
	 * Sets the relationship id.
	 *
	 * @param relationshipId the relationshipId to set
	 */
	public void setRelationshipId(Long relationshipId) {

		this.relationshipId = relationshipId;
	}

	/**
	 * Gets the id extern.
	 *
	 * @return the idExtern
	 */
	public Long getIdExtern() {

		return idExtern;
	}

	/**
	 * Sets the id extern.
	 *
	 * @param idExtern the idExtern to set
	 */
	public void setIdExtern(Long idExtern) {

		this.idExtern = idExtern;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CustomerMemberDTO [communityId=" + communityId + ", organisationId="
				+ organisationId + ", relationshipId=" + relationshipId + ", customerId="
				+ customerId + ", customerRole=" + customerRole + ", idExtern=" + idExtern + "]";
	}

}
