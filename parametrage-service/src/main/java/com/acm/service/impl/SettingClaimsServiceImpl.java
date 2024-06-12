/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingClaimsRepository;
import com.acm.service.SettingClaimsService;
import com.acm.utils.dtos.SettingClaimsDTO;
import com.acm.utils.models.QSettingClaims;
import com.acm.utils.models.SettingClaims;
import com.acm.utils.models.SettingMotifRejets;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

import com.acm.client.CreditClient;
@Service
public class SettingClaimsServiceImpl implements SettingClaimsService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(SettingClaimsServiceImpl.class);

	/** The settingMotifRejets repository. */
	@Autowired
	private SettingClaimsRepository settingClaimsRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;
	
	/** The ib api url. */
	@Value("${ib.environment.apiUrl}")
	private String ibApiUrl;
	
	/** The object mapper. */
	private ObjectMapper objectMapper;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingMotifRejetsService#find(com.acm.utils.dtos.
	 * SettingMotifRejetsDTO)
	 */
	@Override
	public List<SettingClaimsDTO> find(SettingClaimsDTO settingClaimsDTO) {

		// init QSettingMotifRejets
		QSettingClaims qSettingClaims = QSettingClaims.settingClaims;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by enabled
		if (Boolean.TRUE.equals(settingClaimsDTO.getEnabled())) {
			predicate.and(qSettingClaims.enabled.eq(settingClaimsDTO.getEnabled()));
		}

		// find by categorie
		if (!ACMValidationUtils.isNullOrEmpty(settingClaimsDTO.getCategory())) {
			predicate.and(qSettingClaims.category.eq(settingClaimsDTO.getCategory()));
		}

		// find by id
		if (!ACMValidationUtils.isNullOrEmpty(settingClaimsDTO.getId())) {
			predicate.and(qSettingClaims.id.eq(settingClaimsDTO.getId()));
		}

		// QueryDSL using springDATA
		Iterable<SettingClaims> iterable = settingClaimsRepository.findAll(predicate);
		List<SettingClaims> settingClaimss = new ArrayList<>();
		iterable.forEach(settingClaimss::add);
		logger.info("{} : setting Motif Rejets was founded", settingClaimss.size());

		// mapping returned list
		List<SettingClaimsDTO> settingClaimsDTOs = new ArrayList<>();
		settingClaimss.forEach(settingClaims -> settingClaimsDTOs
				.add(mapper.map(settingClaims, SettingClaimsDTO.class)));

		logger.info("Returning founded data ...");
		return settingClaimsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingMotifRejetsService#save(com.acm.utils.dtos.SettingMotifRejetsDTO)
	 */
	@Override
	public SettingClaimsDTO save(SettingClaimsDTO settingClaimsDTO) {

		Preconditions.checkNotNull(settingClaimsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		SettingClaims settingClaims = mapper.map(settingClaimsDTO, SettingClaims.class);
		
		// check code setting if exist

		CommonFunctions.mapperToSave(settingClaims, userClient, logger);
		SettingClaims settingClaim = settingClaimsRepository.save(settingClaims);
		
		try {
			String apiUrl = ibApiUrl +  "parametrage-service/setting-claims/create";
			URI uri = new URI(apiUrl);			
			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + creditClient.loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON);	
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			RestTemplate restTemplate = new RestTemplate();

			HttpEntity<SettingClaimsDTO> request = new HttpEntity<>(settingClaimsDTO, headers);
			ResponseEntity<SettingClaimsDTO> responseAPI = restTemplate.exchange(uri, HttpMethod.POST, request, SettingClaimsDTO.class);
		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
	
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, SettingClaims.class.getSimpleName());
		return mapper.map(settingClaim, SettingClaimsDTO.class);
	}
	
	

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingMotifRejetsService#save(java.lang.Long,
	 * com.acm.utils.dtos.SettingMotifRejetsDTO)
	 */
	@Override
	public SettingClaimsDTO save(Long id, SettingClaimsDTO settingClaimsDTO) {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(settingClaimsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update SettingMotifRejets with ID = {}", id);
		SettingClaims oldSettingClaims = settingClaimsRepository.findById(id).orElse(null);
		// check if object is null

		mapper.map(settingClaimsDTO, oldSettingClaims);
		CommonFunctions.mapperToUpdate(oldSettingClaims, userClient, logger);

		// update & persist data in DB
		SettingClaims newSettingClaims = settingClaimsRepository.save(oldSettingClaims);
		
		try {
			String apiUrl = ibApiUrl +  "parametrage-service/setting-claims/update";
			URI uri = new URI(apiUrl);			
			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + creditClient.loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON);	
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			RestTemplate restTemplate = new RestTemplate();

			HttpEntity<SettingClaimsDTO> request = new HttpEntity<>(settingClaimsDTO, headers);
			ResponseEntity<SettingClaimsDTO> responseAPI = restTemplate.exchange(uri, HttpMethod.PUT, request, SettingClaimsDTO.class);
		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}

		return mapper.map(newSettingClaims, SettingClaimsDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingMotifRejetsService#find()
	 */
	@Override
	public List<SettingClaimsDTO> find() {

		List<SettingClaims> settingClaimss = settingClaimsRepository.findAll();
		List<SettingClaimsDTO> settingClaimsDTOs = new ArrayList<>();
		settingClaimss.forEach(
				setting -> settingClaimsDTOs.add(mapper.map(setting, SettingClaimsDTO.class)));
		return settingClaimsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingMotifRejetsService#delete(java.lang.Long)
	 */
	@Override
	public void delete(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Delete Motif Rejet Type with ID = {}", id);
		SettingClaims oldSettingClaims = settingClaimsRepository.findById(id).orElse(null);

		// check if object is null
		if (oldSettingClaims == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingMotifRejets.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + SettingMotifRejets.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// delete old Motif Rejet type
		settingClaimsRepository.delete(oldSettingClaims);
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE,
				SettingMotifRejets.class.getSimpleName());

	}
}
