/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link LoanApprovalProcessDTO} class (used in transvers-service to load approval process for
 * given LOAN).
 *
 * @author HaythemBenizid
 * @since 1.0.9
 */
public class LoanApprovalProcessDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8103935051166146489L;

	/** The cu loan process ID. */
	private Long cuLoanProcessID;

	/** The cu loan ID. */
	private Long cuLoanID;

	/** The completed. */
	private Long completed;

	/** The loan approval group. */
	private Long loanApprovalGroup;

	/** The customer type. */
	private Long customerType;

	/** The community CU loan ID. */
	private Long communityCULoanID;

	/**
	 * Instantiates a new loan approval process DTO.
	 */
	public LoanApprovalProcessDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new loan approval process DTO.
	 *
	 * @param cuLoanProcessID the cu loan process ID
	 * @param cuLoanID the cu loan ID
	 * @param completed the completed
	 * @param loanApprovalGroup the loan approval group
	 * @param customerType the customer type
	 * @param communityCULoanID the community CU loan ID
	 */
	public LoanApprovalProcessDTO(Long cuLoanProcessID, Long cuLoanID, Long completed,
			Long loanApprovalGroup, Long customerType, Long communityCULoanID) {

		this.cuLoanProcessID = cuLoanProcessID;
		this.cuLoanID = cuLoanID;
		this.completed = completed;
		this.loanApprovalGroup = loanApprovalGroup;
		this.customerType = customerType;
		this.communityCULoanID = communityCULoanID;
	}

	/**
	 * Gets the cu loan process ID.
	 *
	 * @return the cuLoanProcessID
	 */
	public Long getCuLoanProcessID() {

		return cuLoanProcessID;
	}

	/**
	 * Sets the cu loan process ID.
	 *
	 * @param cuLoanProcessID the cuLoanProcessID to set
	 */
	public void setCuLoanProcessID(Long cuLoanProcessID) {

		this.cuLoanProcessID = cuLoanProcessID;
	}

	/**
	 * Gets the cu loan ID.
	 *
	 * @return the cuLoanID
	 */
	public Long getCuLoanID() {

		return cuLoanID;
	}

	/**
	 * Sets the cu loan ID.
	 *
	 * @param cuLoanID the cuLoanID to set
	 */
	public void setCuLoanID(Long cuLoanID) {

		this.cuLoanID = cuLoanID;
	}

	/**
	 * Gets the completed.
	 *
	 * @return the completed
	 */
	public Long getCompleted() {

		return completed;
	}

	/**
	 * Sets the completed.
	 *
	 * @param completed the completed to set
	 */
	public void setCompleted(Long completed) {

		this.completed = completed;
	}

	/**
	 * Gets the loan approval group.
	 *
	 * @return the loanApprovalGroup
	 */
	public Long getLoanApprovalGroup() {

		return loanApprovalGroup;
	}

	/**
	 * Sets the loan approval group.
	 *
	 * @param loanApprovalGroup the loanApprovalGroup to set
	 */
	public void setLoanApprovalGroup(Long loanApprovalGroup) {

		this.loanApprovalGroup = loanApprovalGroup;
	}

	/**
	 * Gets the customer type.
	 *
	 * @return the customerType
	 */
	public Long getCustomerType() {

		return customerType;
	}

	/**
	 * Sets the customer type.
	 *
	 * @param customerType the customerType to set
	 */
	public void setCustomerType(Long customerType) {

		this.customerType = customerType;
	}

	/**
	 * Gets the community CU loan ID.
	 *
	 * @return the communityCULoanID
	 */
	public Long getCommunityCULoanID() {

		return communityCULoanID;
	}

	/**
	 * Sets the community CU loan ID.
	 *
	 * @param communityCULoanID the communityCULoanID to set
	 */
	public void setCommunityCULoanID(Long communityCULoanID) {

		this.communityCULoanID = communityCULoanID;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanApprovalProcessDTO [cuLoanProcessID=" + cuLoanProcessID + ", cuLoanID="
				+ cuLoanID + ", completed=" + completed + ", loanApprovalGroup=" + loanApprovalGroup
				+ ", customerType=" + customerType + ", communityCULoanID=" + communityCULoanID
				+ "]";
	}

}
