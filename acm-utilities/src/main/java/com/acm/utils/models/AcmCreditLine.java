package com.acm.utils.models;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * The Class AcmCreditLine.
 */
@Entity
@Table(name = "ACM_CREDIT_LINE")
public class AcmCreditLine extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4504362878417504150L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_CREDIT_LINE", unique = true, nullable = false)
	private Long id;

	/** The fund name. */
	@Column(name = "FUND_NAME", length = 512)
	private String fundName;

	/** The description. */
	@Column(name = "DESCRIPTION", length = 512)
	private String description;

	/** The balance. */
	@Column(name = "BALANCE")
	private BigDecimal balance;

	/** The control balance. */
	@Column(name = "CONTROL_BALANCE")
	private Boolean controlBalance;

	/** The fund priority. */
	@Column(name = "FUND_PRIORITY")
	private Long fundPriority;

	/** The issue date. */
	@Column(name = "ISSUE_DATE")
	private Date issueDate;

	/** The expiry date. */
	@Column(name = "EXPIRY_DATE")
	private Date expiryDate;

	/** The third party. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_THIRD_PARTY")
	private AcmThirdParty thirdParty;

	/** The products. */
	@ManyToMany(fetch = FetchType.EAGER)
	@JoinTable(name = "ACM_CREDIT_LINE_PRODUCTS",
			joinColumns = {@JoinColumn(name = "ID_CREDIT_LINE")},
			inverseJoinColumns = {@JoinColumn(name = "ID_PRODUCT")})
	private Set<Product> products = new HashSet<>();

	/** The topped up history. */
	@OneToMany(mappedBy = "creditLine", cascade = CascadeType.ALL)
	private Set<AcmToppedUpHistory> toppedUpHistories = new HashSet<>();

	/**
	 * Instantiates a new acm credit line.
	 */
	public AcmCreditLine() {

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
	 * Gets the fund name.
	 *
	 * @return the fund name
	 */
	public String getFundName() {

		return fundName;
	}

	/**
	 * Sets the fund name.
	 *
	 * @param fundName the new fund name
	 */
	public void setFundName(String fundName) {

		this.fundName = fundName;
	}

	/**
	 * Gets the description.
	 *
	 * @return the description
	 */
	public String getDescription() {

		return description;
	}

	/**
	 * Sets the description.
	 *
	 * @param description the new description
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the balance.
	 *
	 * @return the balance
	 */
	public BigDecimal getBalance() {

		return balance;
	}

	/**
	 * Sets the balance.
	 *
	 * @param balance the new balance
	 */
	public void setBalance(BigDecimal balance) {

		this.balance = balance;
	}

	/**
	 * Gets the control balance.
	 *
	 * @return the control balance
	 */
	public Boolean getControlBalance() {

		return controlBalance;
	}

	/**
	 * Sets the control balance.
	 *
	 * @param controlBalance the new control balance
	 */
	public void setControlBalance(Boolean controlBalance) {

		this.controlBalance = controlBalance;
	}

	/**
	 * Gets the fund priority.
	 *
	 * @return the fund priority
	 */
	public Long getFundPriority() {

		return fundPriority;
	}

	/**
	 * Sets the fund priority.
	 *
	 * @param fundPriority the new fund priority
	 */
	public void setFundPriority(Long fundPriority) {

		this.fundPriority = fundPriority;
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
	 * Gets the expiry date.
	 *
	 * @return the expiry date
	 */
	public Date getExpiryDate() {

		return expiryDate;
	}

	/**
	 * Sets the expiry date.
	 *
	 * @param expiryDate the new expiry date
	 */
	public void setExpiryDate(Date expiryDate) {

		this.expiryDate = expiryDate;
	}

	/**
	 * Gets the third party.
	 *
	 * @return the third party
	 */
	public AcmThirdParty getThirdParty() {

		return thirdParty;
	}

	/**
	 * Sets the third party.
	 *
	 * @param thirdParty the new third party
	 */
	public void setThirdParty(AcmThirdParty thirdParty) {

		this.thirdParty = thirdParty;
	}

	/**
	 * Gets the products.
	 *
	 * @return the products
	 */
	public Set<Product> getProducts() {

		return products;
	}

	/**
	 * Sets the products.
	 *
	 * @param products the new products
	 */
	public void setProducts(Set<Product> products) {

		this.products = products;
	}

	/**
	 * Gets the topped up histories.
	 *
	 * @return the topped up histories
	 */
	public Set<AcmToppedUpHistory> getToppedUpHistories() {

		return toppedUpHistories;
	}

	/**
	 * Sets the topped up histories.
	 *
	 * @param toppedUpHistories the new topped up histories
	 */
	public void setToppedUpHistories(Set<AcmToppedUpHistory> toppedUpHistories) {

		this.toppedUpHistories = toppedUpHistories;
	}

}
