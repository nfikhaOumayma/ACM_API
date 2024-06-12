package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import com.acm.utils.models.AcmThirdParty;

/**
 * The Class AcmCreditLineDTO.
 */
public class AcmCreditLineDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3495617856620525134L;

	/** The id. */
	private Long id;

	/** The fund name. */
	private String fundName;

	/** The description. */
	private String description;

	/** The balance. */
	private BigDecimal balance;

	/** The control balance. */
	private Boolean controlBalance;

	/** The fund priority. */
	private Long fundPriority;

	/** The issue date. */
	private Date issueDate;

	/** The expiry date. */
	private Date expiryDate;

	/** The third party. */
	private AcmThirdParty thirdParty;

	/** The products. */
	private Set<ProductDTO> products = new HashSet<>();

	/** The topped up history. */
	private Set<AcmToppedUpHistoryDTO> toppedUpHistories = new HashSet<>();

	/** The enabled. */
	private Boolean enabled;

	/**
	 * Instantiates a new acm credit line DTO.
	 */
	public AcmCreditLineDTO() {

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
	public Set<ProductDTO> getProducts() {

		return products;
	}

	/**
	 * Sets the products.
	 *
	 * @param products the new products
	 */
	public void setProducts(Set<ProductDTO> products) {

		this.products = products;
	}

	/**
	 * Gets the topped up histories.
	 *
	 * @return the topped up histories
	 */
	public Set<AcmToppedUpHistoryDTO> getToppedUpHistories() {

		return toppedUpHistories;
	}

	/**
	 * Sets the topped up histories.
	 *
	 * @param toppedUpHistories the new topped up histories
	 */
	public void setToppedUpHistories(Set<AcmToppedUpHistoryDTO> toppedUpHistories) {

		this.toppedUpHistories = toppedUpHistories;
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

}
