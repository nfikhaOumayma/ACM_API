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
import com.acm.model.rowmapper.PortfolioDTOAbacusRowMapper;
import com.acm.model.rowmapper.UserDTOAbacusRowMapper;
import com.acm.service.UserAbacusService;
import com.acm.utils.dtos.PortfolioDTO;
import com.acm.utils.dtos.UserDTO;

/**
 * {@link UserAbacusServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@Service
public class UserAbacusServiceImpl implements UserAbacusService {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(UserAbacusServiceImpl.class);

	/** The environment. */
	@Autowired
	private Environment environment;

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserAbacusService#find(java.lang.Long)
	 */
	@Override
	public List<UserDTO> find(Long limite) {

		try {
			// load query
			String query = environment.getProperty("query.select.users");
			logger.info("loadUserDTO by limite= {}", limite);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("limite", limite);
			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters,
					new UserDTOAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY ### {}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserAbacusService#find()
	 */
	@Override
	public List<PortfolioDTO> find() {

		try {
			// load query
			String query =
					environment.getProperty("query.select.account.portfolio.CUAccountPortfolio");
			// execute query
			return namedParameterJdbcTemplate.query(query, new PortfolioDTOAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error(
					"### FAILED QUERY : query.select.account.portfolio.CUAccountPortfolio ### {}",
					e);
		}
		// in case of ana error => returning an empty object of portfolios
		return new ArrayList<>();
	}
}
