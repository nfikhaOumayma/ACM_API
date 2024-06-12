package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * The Class AcmToppedUpHistoryDTO.
 */
public class AcmToppedUpHistoryDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3076825236841085553L;

	/** The id. */
	private Long id;

	/** The amount. */
	private BigDecimal amount;

	/** The credit line. */
	private AcmCreditLineDTO creditLine;

	/** The issue date. */
	private Date issueDate;

	/** The enabled. */
	private Boolean enabled;

	/** The updated by. */
	private String updatedBy;

	/** The enabled. */
	private Date dateLastUpdate;

	/**
	 * Instantiates a new acm topped up history DTO.
	 */
	public AcmToppedUpHistoryDTO() {

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
	 * Gets the credit line.
	 *
	 * @return the credit line
	 */
	public AcmCreditLineDTO getCreditLine() {

		return creditLine;
	}

	/**
	 * Sets the credit line.
	 *
	 * @param creditLine the new credit line
	 */
	public void setCreditLine(AcmCreditLineDTO creditLine) {

		this.creditLine = creditLine;
	}

	/**
	 * Gets the issue date.
	 *
	 * @return the issue date
	 */
	public Date getIssueDate() {

		return issueDate;
	}

	/**
	 * Sets the issue date.
	 *
	 * @param issueDate the new issue date
	 */
	public void setIssueDate(Date issueDate) {

		this.issueDate = issueDate;
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

}
