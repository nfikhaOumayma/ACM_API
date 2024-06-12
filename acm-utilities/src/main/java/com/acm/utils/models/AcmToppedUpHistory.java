package com.acm.utils.models;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * The Class AcmToppedUpHistory.
 */
@Entity
@Table(name = "ACM_TOPPED_UP_HISTORY")
public class AcmToppedUpHistory extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 4336359599961164014L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_TOPPED_UP_HISTORY", unique = true, nullable = false)
	private Long id;

	/** The amount. */
	@Column(name = "AMOUNT")
	private BigDecimal amount;

	/** The credit line. */
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "ID_CREDIT_LINE")
	private AcmCreditLine creditLine;

	/** The issue date. */
	@Column(name = "ISSUE_DATE")
	private Date issueDate;

	/**
	 * Instantiates a new acm topped up history.
	 */
	public AcmToppedUpHistory() {

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
	public AcmCreditLine getCreditLine() {

		return creditLine;
	}

	/**
	 * Sets the credit line.
	 *
	 * @param creditLine the new credit line
	 */
	public void setCreditLine(AcmCreditLine creditLine) {

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
}
