/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.json.JSONArray;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.stereotype.Service;

import com.acm.client.ParametrageClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.model.rowmapper.CollectionRowMapper;
import com.acm.service.CollectionAbacusService;
import com.acm.utils.dtos.AcmCollectionDTO;
import com.acm.utils.dtos.CollectionStepDTO;

/**
 * The Class CollectionAbacusServiceImpl.
 */
@Service
public class CollectionAbacusServiceImpl implements CollectionAbacusService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(CollectionAbacusServiceImpl.class);

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The environment. */
	@Autowired
	private Environment environment;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CollectionAbacusService#getCollectionAbacus()
	 */
	@Override
	public Integer initCollectionAbacus() {

		try {
			List<CollectionStepDTO> collectionsStepSettings = new ArrayList<CollectionStepDTO>();
			CollectionStepDTO collectionStepDTO = new CollectionStepDTO();
			// get the fist step setting of each product
			collectionStepDTO.setOrder(0L);
			collectionStepDTO.setEnabled(Boolean.TRUE);
			collectionStepDTO.setProcess(CommonConstants.COLLECTION_CATEGORY);
			collectionsStepSettings = parametrageClient.findSettingCollection(collectionStepDTO);
			JSONObject jsonParam = new JSONObject();
			JSONArray arrayParam = new JSONArray();
			for (CollectionStepDTO collection : collectionsStepSettings) {
				jsonParam.put("productID", collection.getProductId());
				jsonParam.put("MinLateDays", collection.getLateDate());
				jsonParam.put("MinUnpaidAmount", collection.getUnpaidAmount());
				jsonParam.put("MinLoanAmount", collection.getAmount());
				arrayParam.put(jsonParam);
			}

			// load query
			String query = environment.getProperty("query.select.get.collection");
			logger.info("find list of collection query is : = {}", query);
			// param
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("json", arrayParam.toString());
			// execute query
			BigDecimal collectionsAbacus = namedParameterJdbcTemplate.queryForObject(query,
					namedParameters, BigDecimal.class);

			return collectionsAbacus.intValueExact();

		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### query.select.get.collection ### {}", e.getMessage());
		}
		return 0;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CollectionAbacusService#getCollectionFromAbacus(int)
	 */
	@Override
	public AcmCollectionDTO getCollectionFromAbacus(int index) {

		try {
			if (index > 0) {
				// load query
				String query = " select * from dbo.TempCollectionTable where indexrow = :indexColl";
				logger.info("find  collection query is for index = {}", index);
				// param
				SqlParameterSource namedParameters =
						new MapSqlParameterSource().addValue("indexColl", index);
				// execute query
				AcmCollectionDTO collectionsAbacus = namedParameterJdbcTemplate
						.queryForObject(query, namedParameters, new CollectionRowMapper());

				return collectionsAbacus;
			}
			else {
				String query = "DROP TABLE IF EXISTS dbo.TempCollectionTable ";
				logger.info("Drop Collection Temporary Table at the end of Collection Job...");

				namedParameterJdbcTemplate.getJdbcTemplate().execute(query);
			}

		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### query.select.get.collection ### {}", e.getMessage());
		}
		return null;

	}

}
