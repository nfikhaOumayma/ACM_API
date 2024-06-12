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
import com.acm.model.rowmapper.ApplicationFeeDTOAbacusRowMapper;
import com.acm.model.rowmapper.ApplicationFeesDTOAbacusRowMapper;
import com.acm.model.rowmapper.FeeDTOAbacusRowMapper;
import com.acm.service.ApplicationFeeService;
import com.acm.utils.dtos.ApplicationFeeDTO;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link ApplicationFeeServiceImpl} class.
 *
 * @author MoezMhiri
 * @since 1.1.5
 */
@Service
public class ApplicationFeeServiceImpl implements ApplicationFeeService {
	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(ApplicationFeeServiceImpl.class);

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ApplicationFeeService#findApplicationFee(java.lang.Long)
	 */
	@Override
	public Long findApplicationFee(Long idAccount) {

		try {
			// load query
			String query = environment.getProperty("query.select.application.fee");
			logger.info("findFeeRepayment : idAccount= {}", idAccount);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("id_account", idAccount);

			// execute query
			List<Long> applicationFees = namedParameterJdbcTemplate.query(query, namedParameters,
					new ApplicationFeeDTOAbacusRowMapper());
			if (!ACMValidationUtils.isNullOrEmpty(applicationFees)) {
				return applicationFees.get(0);
			}
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return 0L;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ApplicationFeeService#findFees()
	 */
	@Override
	public List<ApplicationFeeDTO> findFees() {

		try {
			// load query
			String query = environment.getProperty("query.select.fees");
			// logger.info("findFees : productType= {}", productType);
			// init params
			SqlParameterSource namedParameters = null;
			// new MapSqlParameterSource().addValue("productType", "%" + productType + "%");

			// execute query
			List<ApplicationFeeDTO> feeLst = namedParameterJdbcTemplate.query(query,
					namedParameters, new ApplicationFeesDTOAbacusRowMapper());
			return feeLst;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	@Override
	public Long checkFees(Long idAccount, List<Long> lstId) {

		try {
			// load query
			String query = environment.getProperty("query.select.check.fee");
			logger.info("Fee : idAccount= {}", idAccount);
			// init params

			SqlParameterSource namedParameters = new MapSqlParameterSource()
					.addValue("id_account", idAccount).addValue("lstIds", lstId);

			// execute query
			List<Long> fees = namedParameterJdbcTemplate.query(query, namedParameters,
					new FeeDTOAbacusRowMapper());
			if (!ACMValidationUtils.isNullOrEmpty(fees)) {
				return fees.get(0);
			}
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return 0L;
	}
}
