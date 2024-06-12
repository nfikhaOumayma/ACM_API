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
import com.acm.model.rowmapper.FinancialAnalysisDTOAbacusRowMapper;
import com.acm.service.FinancialAnalysisAbacusService;
import com.acm.utils.dtos.FinancialAnalysisDTO;

/**
 * {@link CustomerAbacusServiceImpl2} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@Service
public class FinancialAnalysisAbacusServiceImpl implements FinancialAnalysisAbacusService {

	/** logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(FinancialAnalysisAbacusServiceImpl.class);

	/** The environment. */
	@Autowired
	private Environment environment;

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#findByLoan(java.lang.Long)
	 */
	@Override
	public List<FinancialAnalysisDTO> find(Long idLoan) {

		try {
			// load query
			String query = environment.getProperty("query.select.financial.analysis");
			logger.info("findFinancialAnalysis : idLoan= {}", idLoan);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("id_loan", idLoan);
			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters,
					new FinancialAnalysisDTOAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY ### {}", e.getMessage());
		}
		return new ArrayList<>();
	}
}
