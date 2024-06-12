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
import com.acm.model.rowmapper.AddressDTOAbacusRowMapper;
import com.acm.model.rowmapper.AddressListDTOAbacusRowMapper;
import com.acm.model.rowmapper.AddressListValueDTOAbacusRowMapper;
import com.acm.model.rowmapper.AddressSettingDTOAbacusRowMapper;
import com.acm.model.rowmapper.AddressTypeDTOAbacusRowMapper;
import com.acm.service.AddressSettingAbacusService;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.AddressListDTO;
import com.acm.utils.dtos.AddressListValueDTO;
import com.acm.utils.dtos.AddressSettingAbacusDTO;
import com.acm.utils.dtos.AddressTypeDTO;

/**
 * {@link AddressSettingAbacusServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Service
public class AddressSettingAbacusServiceImpl implements AddressSettingAbacusService {

	/** logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(AddressSettingAbacusServiceImpl.class);

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
	 * @see com.acm.service.AddressSettingAbacusService#findAddressType()
	 */
	@Override
	public List<AddressTypeDTO> findAddressType() {

		try {
			// load query
			String query = environment.getProperty("query.select.settings.address.AddressType");

			// execute query
			List<AddressTypeDTO> addressTypeDTOs =
					namedParameterJdbcTemplate.query(query, new AddressTypeDTOAbacusRowMapper());
			logger.info("Find ADDRESS_TYPE data from ABACUS DB :: DONE");
			// returning result
			return addressTypeDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressSettingAbacusService#findAddressList()
	 */
	@Override
	public List<AddressListDTO> findAddressList() {

		try {
			// load query
			String query = environment.getProperty("query.select.settings.address.AddressList");

			// execute query
			List<AddressListDTO> addressListDTOs =
					namedParameterJdbcTemplate.query(query, new AddressListDTOAbacusRowMapper());
			logger.info("Find ADDRESS_LIST data from ABACUS DB :: DONE");
			// returning result
			return addressListDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressSettingAbacusService#findAddressListValue()
	 */
	@Override
	public List<AddressListValueDTO> findAddressListValue() {

		try {
			// load query
			String query =
					environment.getProperty("query.select.settings.address.AddressListValue");

			// execute query
			List<AddressListValueDTO> addressListValueDTOs = namedParameterJdbcTemplate.query(query,
					new AddressListValueDTOAbacusRowMapper());
			logger.info("Find ADDRESS_LIST_VALUE data from ABACUS DB :: DONE");
			// returning result
			return addressListValueDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressSettingAbacusService#findSettingsAddress()
	 */
	@Override
	public List<AddressSettingAbacusDTO> findSettingsAddress() {

		try {
			// load query
			String query = environment.getProperty("query.select.settings.address.SettingsAddress");

			// execute query
			List<AddressSettingAbacusDTO> addressSettingsDTOs =
					namedParameterJdbcTemplate.query(query, new AddressSettingDTOAbacusRowMapper());
			logger.info("Find ADDRESS_SETTINGS data from ABACUS DB :: DONE");
			// returning result
			return addressSettingsDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressSettingAbacusService#loadAddressForCustomer(java.lang.Long)
	 */
	@Override
	public List<AddressDTO> loadAddressForCustomer(Long limite) {

		try {
			// load query
			String query = environment.getProperty("query.select.settings.address.load");
			logger.info("loadAddressForCustomer : limite= {}", limite);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("limite", limite);

			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters,
					new AddressDTOAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressSettingAbacusService#loadAddressByCustomer(java.lang.Long)
	 */
	@Override
	public List<AddressDTO> loadAddressByCustomer(Long idCustomer) {

		try {
			// load query
			String query = environment.getProperty("query.select.address.by.customer");
			logger.info("loadAddressByCustomer : idCustomer= {}", idCustomer);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_CUSTOMER_CONSTANT, idCustomer);

			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters,
					new AddressDTOAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
		}
		return new ArrayList<>();
	}
}
