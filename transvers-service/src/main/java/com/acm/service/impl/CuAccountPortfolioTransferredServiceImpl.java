/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.stereotype.Service;

import com.acm.constants.common.CommonLoggerMessage;
import com.acm.model.rowmapper.CUAccountPortfolioTransferredRowMapper;
import com.acm.model.rowmapper.CustomerActiveAccountAbacusRowMapper;
import com.acm.service.CuAccountPortfolioTransferredService;
import com.acm.utils.dtos.CUAccountPortfolioTransferredDTO;

/**
 * The Class CuAccountPortfolioTransferredServiceImpl.
 */
@Service
public class CuAccountPortfolioTransferredServiceImpl
		implements CuAccountPortfolioTransferredService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(CuAccountPortfolioTransferredServiceImpl.class);

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The Environment. */
	@Autowired
	private Environment environment;
	/** The Constant ID_CUSTOMER_CONSTANT. */
	private static final String LAST_CUACCOUNT_PORTFOLIO_TRANSFERRED_ID =
			"lastCuAccountPortfolioTransferId";

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CuAccountPortfolioTransferredService#find(java.lang.Long)
	 */
	@Override
	public List<CUAccountPortfolioTransferredDTO> find(Long lastCuAccountPortfolioTransferred) {

		try {
			// load query
			String query = environment.getProperty("query.select.CUAccountPortfolioTransferred");
			logger.info("find CUAccountPortfolioTransferred after id = {}",
					lastCuAccountPortfolioTransferred);
			// init params
			SqlParameterSource namedParameters = new MapSqlParameterSource().addValue(
					LAST_CUACCOUNT_PORTFOLIO_TRANSFERRED_ID, lastCuAccountPortfolioTransferred);
			// execute query
			List<CUAccountPortfolioTransferredDTO> accountPortfolioTransferredDTOs =
					namedParameterJdbcTemplate.query(query, namedParameters,
							new CUAccountPortfolioTransferredRowMapper());

			return accountPortfolioTransferredDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.CUAccountPortfolioTransferred ### {}",
					e.getMessage());
		}
		// in case of ana error => returning an empty object of customer with id extern = 0
		return new ArrayList<>();
	}

	@Override
	public Long findLast() {

		try {
			// load query
			String query =
					environment.getProperty("query.select.last.CUAccountPortfolioTransferred");
			logger.info(
					"find lastCUAccountPortfolioTransferred LAST_CUACCOUNT_PORTFOLIO_TRANSFERRED_ID");

			// execute query
			return namedParameterJdbcTemplate
					.query(query, new CustomerActiveAccountAbacusRowMapper()).get(0)
					.getLong(LAST_CUACCOUNT_PORTFOLIO_TRANSFERRED_ID);

		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error(
					"### FAILED QUERY : query.select.last.CUAccountPortfolioTransferred ### {}",
					e.getMessage());
		}
		return 0L;
	}
}
