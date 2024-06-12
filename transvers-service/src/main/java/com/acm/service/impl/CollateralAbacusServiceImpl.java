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
import com.acm.model.rowmapper.AcmCollateralDTOAbacusRowMapper;
import com.acm.model.rowmapper.CollateralDTOAbacusRowMapper;
import com.acm.service.CollateralAbacusService;
import com.acm.utils.dtos.AcmCollateralDTO;
import com.acm.utils.dtos.CollaterolDTO;

/**
 * {@link CollateralAbacusServiceImpl} class.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
@Service
public class CollateralAbacusServiceImpl implements CollateralAbacusService {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(CollateralAbacusServiceImpl.class);

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CollateralAbacusService#find(java.lang.Long)
	 */
	@Override
	public List<CollaterolDTO> find(Long idLoan) {

		try {
			// load query
			String query = environment.getProperty("query.select.loan.collateral");
			logger.info("idLoan= {}", idLoan);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("id_loan", idLoan);
			// execute query
			List<CollaterolDTO> collaterolDTOs = namedParameterJdbcTemplate.query(query,
					namedParameters, new CollateralDTOAbacusRowMapper());
			logger.info(" find COLLATEROL data from ABACUS DB :: DONE");
			// returning result
			return collaterolDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CollateralAbacusService#findActiveAndInactiveCollaterols(java.util.List)
	 */
	@Override
	public List<AcmCollateralDTO> findActiveAndInactiveCollaterols(List<Long> idLoans) {

		try {
			// load query
			String query = environment.getProperty("query.select.all.loan.collateral");
			logger.info("idLoan in : {}", idLoans);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("id_loans", idLoans);
			// execute query
			List<AcmCollateralDTO> collaterolDTOs = namedParameterJdbcTemplate.query(query,
					namedParameters, new AcmCollateralDTOAbacusRowMapper());
			logger.info(" find Active and Inactive COLLATEROL data from ABACUS DB :: DONE");
			// returning result
			return collaterolDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}
}
