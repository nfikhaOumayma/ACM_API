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
import com.acm.model.rowmapper.LoanProcessSettingDTOAbacusRowMapper;
import com.acm.service.LoanProcessSettingAbacusService;
import com.acm.utils.dtos.LoanProcessSettingDTO;

/**
 * {@link LoanProcessSettingAbacusServiceImpl} class.
 *
 * @author RadhouaneHomrani
 * @since 0.2.0
 */
@Service
public class LoanProcessSettingAbacusServiceImpl implements LoanProcessSettingAbacusService {

	/** logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(LoanProcessSettingAbacusServiceImpl.class);

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanAbacusService#find(java.lang.Long)
	 */
	@Override
	public List<LoanProcessSettingDTO> find(Long productId) {

		try {
			// load query
			String query = environment.getProperty("query.select.loan.process.setting");
			logger.info("findLoanProcessSetting by productId= {}", productId);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("id_product", productId);
			// execute query
			List<LoanProcessSettingDTO> loanProcessDTOs = namedParameterJdbcTemplate.query(query,
					namedParameters, new LoanProcessSettingDTOAbacusRowMapper());

			logger.info(" find LOAN PROCESS SETTING data from ABACUS DB :: DONE");
			// returning sorted list by IdLoanExtern
			return loanProcessDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY ### {}", e.getMessage());
		}
		return new ArrayList<>();
	}
}
