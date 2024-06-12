/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link CUAccountPortfolioTransferredDTO } class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class CUAccountPortfolioTransferredDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -747006403030899798L;

	/** The id. */
	private Long id;

	/** The to porfolio. */
	private Long toPorfolio;

	/** The from portfolio. */
	private Long fromPortfolio;

	/** The transfered date. */
	private Date transferedDate;

	/** The customer id. */
	private Long customerId;

	/** The product type. */
	private String productType;

	/** The to porfolio code. */
	private String toPorfolioCode;

	/** The to porfolio name. */
	private String toPorfolioName;

	/** The cu account id. */
	private Long cuAccountId;

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
	 * Gets the to porfolio.
	 *
	 * @return the to porfolio
	 */
	public Long getToPorfolio() {

		return toPorfolio;
	}

	/**
	 * Sets the to porfolio.
	 *
	 * @param toPorfolio the new to porfolio
	 */
	public void setToPorfolio(Long toPorfolio) {

		this.toPorfolio = toPorfolio;
	}

	/**
	 * Gets the from portfolio.
	 *
	 * @return the from portfolio
	 */
	public Long getFromPortfolio() {

		return fromPortfolio;
	}

	/**
	 * Sets the from portfolio.
	 *
	 * @param fromPortfolio the new from portfolio
	 */
	public void setFromPortfolio(Long fromPortfolio) {

		this.fromPortfolio = fromPortfolio;
	}

	/**
	 * Gets the transfered date.
	 *
	 * @return the transfered date
	 */
	public Date getTransferedDate() {

		return transferedDate;
	}

	/**
	 * Sets the transfered date.
	 *
	 * @param transferedDate the new transfered date
	 */
	public void setTransferedDate(Date transferedDate) {

		this.transferedDate = transferedDate;
	}

	/**
	 * Gets the customer id.
	 *
	 * @return the customer id
	 */
	public Long getCustomerId() {

		return customerId;
	}

	/**
	 * Sets the customer id.
	 *
	 * @param customerId the new customer id
	 */
	public void setCustomerId(Long customerId) {

		this.customerId = customerId;
	}

	/**
	 * Gets the product type.
	 *
	 * @return the product type
	 */
	public String getProductType() {

		return productType;
	}

	/**
	 * Sets the product type.
	 *
	 * @param productType the new product type
	 */
	public void setProductType(String productType) {

		this.productType = productType;
	}

	/**
	 * Gets the cu account id.
	 *
	 * @return the cu account id
	 */
	public Long getCuAccountId() {

		return cuAccountId;
	}

	/**
	 * Sets the cu account id.
	 *
	 * @param cuAccountId the new cu account id
	 */
	public void setCuAccountId(Long cuAccountId) {

		this.cuAccountId = cuAccountId;
	}

	/**
	 * Gets the to porfolio code.
	 *
	 * @return the to porfolio code
	 */
	public String getToPorfolioCode() {

		return toPorfolioCode;
	}

	/**
	 * Sets the to porfolio code.
	 *
	 * @param toPorfolioCode the new to porfolio code
	 */
	public void setToPorfolioCode(String toPorfolioCode) {

		this.toPorfolioCode = toPorfolioCode;
	}

	/**
	 * Gets the to porfolio name.
	 *
	 * @return the to porfolio name
	 */
	public String getToPorfolioName() {

		return toPorfolioName;
	}

	/**
	 * Sets the to porfolio name.
	 *
	 * @param toPorfolioName the new to porfolio name
	 */
	public void setToPorfolioName(String toPorfolioName) {

		this.toPorfolioName = toPorfolioName;
	}

}
