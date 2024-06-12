/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link LoanStatutDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.4.0
 */
public class LoanStatutDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5520120043771590395L;

	/** The status 1 new. */
	private Long status1New;

	/** The status 2 drafts. */
	private Long status2Drafts;

	/** The status 3 awaiting approval. */
	private Long status3AwaitingApproval;

	/** The status 4 approved. */
	private Long status4Approved;

	/** The status 5 rejected. */
	private Long status5Rejected;

	/** The status 6 cancelled. */
	private Long status6Cancelled;

	/** The status 7 correctifs. */
	private Long status7Correctifs;

	/** The my task count. */
	private Long myTaskCount;

	/**
	 * Instantiates a new loan statut DTO.
	 */
	public LoanStatutDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new loan statut DTO.
	 *
	 * @param status1New the status 1 new
	 * @param status2Drafts the status 2 drafts
	 * @param status3AwaitingApproval the status 3 awaiting approval
	 * @param status4Approved the status 4 approved
	 * @param status5Rejected the status 5 rejected
	 * @param status6Cancelled the status 6 cancelled
	 * @param status7Correctifs the status 7 correctifs
	 * @param myTaskCount the my task count
	 */
	public LoanStatutDTO(Long status1New, Long status2Drafts, Long status3AwaitingApproval,
			Long status4Approved, Long status5Rejected, Long status6Cancelled,
			Long status7Correctifs, Long myTaskCount) {

		this.status1New = status1New;
		this.status2Drafts = status2Drafts;
		this.status3AwaitingApproval = status3AwaitingApproval;
		this.status4Approved = status4Approved;
		this.status5Rejected = status5Rejected;
		this.status6Cancelled = status6Cancelled;
		this.status7Correctifs = status7Correctifs;
		this.myTaskCount = myTaskCount;
	}

	/**
	 * Gets the status 1 new.
	 *
	 * @return the status1New
	 */
	public Long getStatus1New() {

		return status1New;
	}

	/**
	 * Sets the status 1 new.
	 *
	 * @param status1New the status1New to set
	 */
	public void setStatus1New(Long status1New) {

		this.status1New = status1New;
	}

	/**
	 * Gets the status 2 drafts.
	 *
	 * @return the status2Drafts
	 */
	public Long getStatus2Drafts() {

		return status2Drafts;
	}

	/**
	 * Sets the status 2 drafts.
	 *
	 * @param status2Drafts the status2Drafts to set
	 */
	public void setStatus2Drafts(Long status2Drafts) {

		this.status2Drafts = status2Drafts;
	}

	/**
	 * Gets the status 3 awaiting approval.
	 *
	 * @return the status3AwaitingApproval
	 */
	public Long getStatus3AwaitingApproval() {

		return status3AwaitingApproval;
	}

	/**
	 * Sets the status 3 awaiting approval.
	 *
	 * @param status3AwaitingApproval the status3AwaitingApproval to set
	 */
	public void setStatus3AwaitingApproval(Long status3AwaitingApproval) {

		this.status3AwaitingApproval = status3AwaitingApproval;
	}

	/**
	 * Gets the status 4 approved.
	 *
	 * @return the status4Approved
	 */
	public Long getStatus4Approved() {

		return status4Approved;
	}

	/**
	 * Sets the status 4 approved.
	 *
	 * @param status4Approved the status4Approved to set
	 */
	public void setStatus4Approved(Long status4Approved) {

		this.status4Approved = status4Approved;
	}

	/**
	 * Gets the status 5 rejected.
	 *
	 * @return the status5Rejected
	 */
	public Long getStatus5Rejected() {

		return status5Rejected;
	}

	/**
	 * Sets the status 5 rejected.
	 *
	 * @param status5Rejected the status5Rejected to set
	 */
	public void setStatus5Rejected(Long status5Rejected) {

		this.status5Rejected = status5Rejected;
	}

	/**
	 * Gets the status 6 cancelled.
	 *
	 * @return the status6Cancelled
	 */
	public Long getStatus6Cancelled() {

		return status6Cancelled;
	}

	/**
	 * Sets the status 6 cancelled.
	 *
	 * @param status6Cancelled the status6Cancelled to set
	 */
	public void setStatus6Cancelled(Long status6Cancelled) {

		this.status6Cancelled = status6Cancelled;
	}

	/**
	 * Gets the status 7 correctifs.
	 *
	 * @return the status7Correctifs
	 */
	public Long getStatus7Correctifs() {

		return status7Correctifs;
	}

	/**
	 * Sets the status 7 correctifs.
	 *
	 * @param status7Correctifs the status7Correctifs to set
	 */
	public void setStatus7Correctifs(Long status7Correctifs) {

		this.status7Correctifs = status7Correctifs;
	}

	/**
	 * Gets the my task count.
	 *
	 * @return the myTaskCount
	 */
	public Long getMyTaskCount() {

		return myTaskCount;
	}

	/**
	 * Sets the my task count.
	 *
	 * @param myTaskCount the myTaskCount to set
	 */
	public void setMyTaskCount(Long myTaskCount) {

		this.myTaskCount = myTaskCount;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanStatutDTO [status1New=" + status1New + ", status2Drafts=" + status2Drafts
				+ ", status3AwaitingApproval=" + status3AwaitingApproval + ", status4Approved="
				+ status4Approved + ", status5Rejected=" + status5Rejected + ", status6Cancelled="
				+ status6Cancelled + ", status7Correctifs=" + status7Correctifs + "]";
	}
}
