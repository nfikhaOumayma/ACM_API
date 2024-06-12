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
import com.acm.model.rowmapper.UDFFieldsAbacusRowMapper;
import com.acm.model.rowmapper.UDFFieldsLinksCustomerAbacusRowMapper;
import com.acm.model.rowmapper.UDFFieldsLinksLoanAbacusRowMapper;
import com.acm.model.rowmapper.UDFGroupAbacusRowMapper;
import com.acm.model.rowmapper.UDFListAbacusRowMapper;
import com.acm.model.rowmapper.UDFListValuesAbacusRowMapper;
import com.acm.service.UDFSettingAbacusService;
import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.dtos.UserDefinedFieldListValuesDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;

/**
 * {@link UDFSettingAbacusServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Service
public class UDFSettingAbacusServiceImpl implements UDFSettingAbacusService {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(UDFSettingAbacusServiceImpl.class);

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The Constant ID_CUSTOMER_CONSTANT. */
	private static final String ID_CUSTOMER_CONSTANT = "id_customer";

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UDFSettingAbacusService#findUserDefinedFields()
	 */
	@Override
	public List<UserDefinedFieldsDTO> findUserDefinedFields() {

		try {
			// load query
			String query = environment.getProperty("query.select.settings.udf.UserDefinedFields");

			// execute query
			List<UserDefinedFieldsDTO> userDefinedFieldsDTOs =
					namedParameterJdbcTemplate.query(query, new UDFFieldsAbacusRowMapper());
			logger.info("Find  UserDefinedFields data from ABACUS DB :: DONE");
			// returning result
			return userDefinedFieldsDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UDFSettingAbacusService#findUserDefinedFieldGroup()
	 */
	@Override
	public List<UserDefinedFieldGroupDTO> findUserDefinedFieldGroup() {

		try {
			// load query
			String query =
					environment.getProperty("query.select.settings.udf.UserDefinedFieldGroup");

			// execute query
			List<UserDefinedFieldGroupDTO> userDefinedFieldGroupDTOs =
					namedParameterJdbcTemplate.query(query, new UDFGroupAbacusRowMapper());
			logger.info("Find UserDefinedFieldGroup data from ABACUS DB :: DONE");
			// returning result
			return userDefinedFieldGroupDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UDFSettingAbacusService#findUserDefinedFieldListValues()
	 */
	@Override
	public List<UserDefinedFieldListValuesDTO> findUserDefinedFieldListValues() {

		try {
			// load query
			String queryList =
					environment.getProperty("query.select.settings.udf.UserDefinedFieldLists");
			String queryListValues =
					environment.getProperty("query.select.settings.udf.UserDefinedFieldListValues");

			// execute query
			List<UserDefinedFieldListValuesDTO> userDefinedFieldListValuesDTOs =
					namedParameterJdbcTemplate.query(queryList, new UDFListAbacusRowMapper());
			logger.info("Find userDefinedFieldList data from ABACUS DB :: DONE");
			// execute query
			userDefinedFieldListValuesDTOs.addAll(namedParameterJdbcTemplate.query(queryListValues,
					new UDFListValuesAbacusRowMapper()));
			logger.info("Find userDefinedFieldListValues data from ABACUS DB :: DONE");
			// returning result
			return userDefinedFieldListValuesDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UDFSettingAbacusService#loadUDFForLoan(java.lang.Long)
	 */
	@Override
	public List<UserDefinedFieldsLinksDTO> loadUDFForLoan(Long limite) {

		try {
			// load query
			String query = environment.getProperty("query.select.udf.loan");
			logger.info("loadUDFForLoan by limite= {}", limite);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("limite", limite);

			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters,
					new UDFFieldsLinksLoanAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UDFSettingAbacusService#loadUDFByLoan(java.lang.Long)
	 */
	@Override
	public List<UserDefinedFieldsLinksDTO> loadUDFByLoan(Long idAccountExtern) {

		try {
			// load query
			String query = environment.getProperty("query.select.udf.by.loan");
			logger.info("loadUDFByLoan : idAccountExtern= {}", idAccountExtern);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("cu_account_id", idAccountExtern);

			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters,
					new UDFFieldsLinksLoanAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UDFSettingAbacusService#loadUDFForCustomer(java.lang.Long)
	 */
	@Override
	public List<UserDefinedFieldsLinksDTO> loadUDFForCustomer(Long limite) {

		try {
			// load query
			String query = environment.getProperty("query.select.udf.customer");
			logger.info("loadUDFForCustomer by limite= {}", limite);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("limite", limite);

			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters,
					new UDFFieldsLinksCustomerAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UDFSettingAbacusService#loadUDFByCustomer(java.lang.Long)
	 */
	@Override
	public List<UserDefinedFieldsLinksDTO> loadUDFByCustomer(Long idCustomer) {

		try {
			// load query
			String query = environment.getProperty("query.select.udf.by.customer");
			logger.info("loadUDFByCustomer : idCustomer= {}", idCustomer);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_CUSTOMER_CONSTANT, idCustomer);

			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters,
					new UDFFieldsLinksCustomerAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
		}
		return new ArrayList<>();
	}
}
