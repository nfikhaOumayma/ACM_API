/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

/**
 * The Class PortfolioDTO.
 * 
 * @author Salmen Fatnassi
 * @since 1.1.2
 */
public class PortfolioDTO extends GenericDTO implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 6728424767167473236L;

	/** The portfolio id. */
	private Long portfolioId;

	/** The portfolio name. */
	private String portfolioName;

	/** The enable. */
	private Boolean enable;

	/**
	 * Instantiates a new portfolio DTO.
	 */
	public PortfolioDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new portfolio DTO.
	 *
	 * @param portfolioId the portfolio id
	 * @param portfolioName the portfolio name
	 * @param enable the enable
	 */
	public PortfolioDTO(Long portfolioId, String portfolioName, Boolean enable) {

		super();
		this.portfolioId = portfolioId;
		this.portfolioName = portfolioName;
		this.enable = enable;
	}

	/**
	 * Gets the portfolio id.
	 *
	 * @return the portfolio id
	 */
	public Long getPortfolioId() {

		return portfolioId;
	}

	/**
	 * Sets the portfolio id.
	 *
	 * @param portfolioId the new portfolio id
	 */
	public void setPortfolioId(Long portfolioId) {

		this.portfolioId = portfolioId;
	}

	/**
	 * Gets the portfolio name.
	 *
	 * @return the portfolio name
	 */
	public String getPortfolioName() {

		return portfolioName;
	}

	/**
	 * Sets the portfolio name.
	 *
	 * @param portfolioName the new portfolio name
	 */
	public void setPortfolioName(String portfolioName) {

		this.portfolioName = portfolioName;
	}

	/**
	 * Gets the enable.
	 *
	 * @return the enable
	 */
	public Boolean getEnable() {

		return enable;
	}

	/**
	 * Sets the enable.
	 *
	 * @param enable the new enable
	 */
	public void setEnable(Boolean enable) {

		this.enable = enable;
	}

}
