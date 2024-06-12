/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link LoanProcessSettingDTO} class.
 *
 * @author RadhouaneHomrani
 * @since 0.2.0
 */
public class LoanProcessSettingDTO extends GenericDTO implements Serializable {

	/**
	 * The Constant serialVersionUID.
	 */
	private static final long serialVersionUID = 949369638418163690L;

	/** The productid. */
	private Long productId;

	/** Product Code. */
	private String productCode;

	/** Loan Process Code. */
	private String productProcessCode;

	/** Loan Process Description. */
	private String productProcessDescription;

	/** Loan Process Required. */
	private Boolean productProcessRequire;

	/** The product loan processes ID. */
	private Integer productLoanProcessesID;

	/** The menu key. */
	private String menuKey;

	/** The apply. */
	private Boolean apply;

	/** The approve. */
	private Boolean approve;

	/**
	 * Instantiates a new loan Process.
	 */
	public LoanProcessSettingDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new loan process setting DTO.
	 *
	 * @param productId the product id
	 * @param productCode the product code
	 * @param productProcessCode the product process code
	 * @param productProcessDescription the product process description
	 * @param productProcessRequire the product process require
	 * @param productLoanProcessesID the product loan processes ID
	 * @param menuKey the menu key
	 * @param apply the apply
	 * @param approve the approve
	 */
	public LoanProcessSettingDTO(Long productId, String productCode, String productProcessCode,
			String productProcessDescription, Boolean productProcessRequire,
			Integer productLoanProcessesID, String menuKey, Boolean apply, Boolean approve) {

		this.productId = productId;
		this.productCode = productCode;
		this.productProcessCode = productProcessCode;
		this.productProcessDescription = productProcessDescription;
		this.productProcessRequire = productProcessRequire;
		this.productLoanProcessesID = productLoanProcessesID;
		this.menuKey = menuKey;
		this.apply = apply;
		this.approve = approve;
	}

	/**
	 * Gets the product id.
	 *
	 * @return the product id
	 */
	public Long getProductId() {

		return productId;
	}

	/**
	 * Sets the product id.
	 *
	 * @param productId the new product id
	 */
	public void setProductId(Long productId) {

		this.productId = productId;
	}

	/**
	 * Gets the product code.
	 *
	 * @return the product code
	 */
	public String getProductCode() {

		return productCode;
	}

	/**
	 * Sets the product code.
	 *
	 * @param productCode the new product code
	 */
	public void setProductCode(String productCode) {

		this.productCode = productCode;
	}

	/**
	 * Gets the product process code.
	 *
	 * @return the product process code
	 */
	public String getProductProcessCode() {

		return productProcessCode;
	}

	/**
	 * Sets the product process code.
	 *
	 * @param productProcessCode the new product process code
	 */
	public void setProductProcessCode(String productProcessCode) {

		this.productProcessCode = productProcessCode;
	}

	/**
	 * Gets the product process description.
	 *
	 * @return the product process description
	 */
	public String getProductProcessDescription() {

		return productProcessDescription;
	}

	/**
	 * Sets the product process description.
	 *
	 * @param productProcessDescription the new product process description
	 */
	public void setProductProcessDescription(String productProcessDescription) {

		this.productProcessDescription = productProcessDescription;
	}

	/**
	 * Gets the product process require.
	 *
	 * @return the productProcessRequire
	 */
	public Boolean getProductProcessRequire() {

		return productProcessRequire;
	}

	/**
	 * Sets the product process require.
	 *
	 * @param productProcessRequire the productProcessRequire to set
	 */
	public void setProductProcessRequire(Boolean productProcessRequire) {

		this.productProcessRequire = productProcessRequire;
	}

	/**
	 * Gets the product loan processes ID.
	 *
	 * @return the productLoanProcessesID
	 */
	public Integer getProductLoanProcessesID() {

		return productLoanProcessesID;
	}

	/**
	 * Sets the product loan processes ID.
	 *
	 * @param productLoanProcessesID the productLoanProcessesID to set
	 */
	public void setProductLoanProcessesID(Integer productLoanProcessesID) {

		this.productLoanProcessesID = productLoanProcessesID;
	}

	/**
	 * Gets the menu key.
	 *
	 * @return the menuKey
	 */
	public String getMenuKey() {

		return menuKey;
	}

	/**
	 * Sets the menu key.
	 *
	 * @param menuKey the menuKey to set
	 */
	public void setMenuKey(String menuKey) {

		this.menuKey = menuKey;
	}

	/**
	 * Gets the apply.
	 *
	 * @return the apply
	 */
	public Boolean getApply() {

		return apply;
	}

	/**
	 * Sets the apply.
	 *
	 * @param apply the apply to set
	 */
	public void setApply(Boolean apply) {

		this.apply = apply;
	}

	/**
	 * Gets the approve.
	 *
	 * @return the approve
	 */
	public Boolean getApprove() {

		return approve;
	}

	/**
	 * Sets the approve.
	 *
	 * @param approve the approve to set
	 */
	public void setApprove(Boolean approve) {

		this.approve = approve;
	}

}
