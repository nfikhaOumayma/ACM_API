/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.AssetRepository;
import com.acm.service.AssetService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AssetDTO;
import com.acm.utils.dtos.pagination.AssetPaginationDTO;
import com.acm.utils.models.Address;
import com.acm.utils.models.Asset;
import com.acm.utils.models.QAsset;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link AssetServiceImpl } class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Service
public class AssetServiceImpl implements AssetService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AssetServiceImpl.class);

	/** The asset repository. */
	@Autowired
	private AssetRepository assetRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AssetService#find(com.acm.utils.dtos.AssetDTO)
	 */
	@Override
	public List<AssetDTO> find(AssetDTO assetDTO) {

		Preconditions.checkNotNull(assetDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// init QAddress
		QAsset qAsset = QAsset.asset;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qAsset.enabled.eq(Boolean.TRUE));

		// find by supplier id
		if (!ACMValidationUtils.isNullOrEmpty(assetDTO.getSupplier())
				&& (!ACMValidationUtils.isNullOrEmpty(assetDTO.getSupplier().getId()))) {
			predicate.and(qAsset.supplier.id.eq(assetDTO.getSupplier().getId()));
		} // find by supplier name
		if (!ACMValidationUtils.isNullOrEmpty(assetDTO.getSupplierName())) {
			predicate.and(qAsset.supplier.name.eq(assetDTO.getSupplierName()));
		}
		predicate.and(qAsset.supplier.status.notEqualsIgnoreCase("NON CONTRACTED"));

		Iterable<Asset> iterable = assetRepository.findAll(predicate);
		List<Asset> assets = new ArrayList<>();
		iterable.forEach(assets::add);
		logger.info("{} : assets was founded", assets.size());

		List<AssetDTO> assetDTOs = new ArrayList<>();
		assets.forEach(asset -> assetDTOs.add(mapper.map(asset, AssetDTO.class)));
		return assetDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AssetService#save(com.acm.utils.dtos.AssetDTO)
	 */
	@Override
	public AssetDTO save(AssetDTO assetDTO) {

		Preconditions.checkNotNull(assetDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Asset asset = mapper.map(assetDTO, Asset.class);

		CommonFunctions.mapperToSave(asset, userClient, logger);
		Asset newAsset = assetRepository.save(asset);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, Asset.class.getSimpleName());
		return mapper.map(newAsset, AssetDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AssetService#saveAll(java.util.List)
	 */
	@Override
	public List<AssetDTO> saveAll(List<AssetDTO> assetDTOs) {

		logger.info("Begin save all assets...");
		List<Asset> assets = new ArrayList<>();
		List<AssetDTO> assetDTOResults = new ArrayList<>();
		for (AssetDTO assetDTO : assetDTOs) {
			Asset asset = mapper.map(assetDTO, Asset.class);
			CommonFunctions.mapperToSave(asset, userClient, logger);
			assets.add(asset);
		}
		if (!ACMValidationUtils.isNullOrEmpty(assets)) {
			assetRepository.saveAll(assets)
					.forEach(a -> assetDTOResults.add(mapper.map(a, AssetDTO.class)));
		}
		return assetDTOResults;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AssetService#save(java.lang.Long, com.acm.utils.dtos.AssetDTO)
	 */
	@Override
	public AssetDTO save(Long id, AssetDTO assetDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(assetDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update asset with ID = {}", id);
		Asset oldAsset = assetRepository.findById(id).orElse(null);

		// check if object is null
		if (oldAsset == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Asset.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Address.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldAddress)
		mapper.map(assetDTO, oldAsset);
		CommonFunctions.mapperToUpdate(oldAsset, userClient, logger);

		// update & persist data in DB
		Asset newAsset = assetRepository.save(oldAsset);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Asset.class.getSimpleName());
		return mapper.map(newAsset, AssetDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AssetService#find(com.acm.utils.dtos.pagination.AssetPaginationDTO)
	 */
	@Override
	public AssetPaginationDTO find(AssetPaginationDTO assetPaginationDTO) {

		Preconditions.checkNotNull(assetPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(assetPaginationDTO.getPageNumber())) {
			assetPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(assetPaginationDTO.getPageSize())) {
			assetPaginationDTO.setPageSize(10);
		}
		// setting default data
		assetPaginationDTO.setResultsAssets(new ArrayList<>());
		// setting default totals pages
		assetPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		assetPaginationDTO.setTotalPages(0);
		// init QCustomer
		QAsset qAsset = QAsset.asset;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qAsset.enabled.eq(Boolean.TRUE));

		// find LIKE CustomerNumber
		if (!ACMValidationUtils.isNullOrEmpty(assetPaginationDTO.getParams().getCodeArticle())) {
			predicate.and(qAsset.codeArticle
					.like("%" + assetPaginationDTO.getParams().getCodeArticle() + "%"));
		}

		// find LIKE customerName
		if (!ACMValidationUtils.isNullOrEmpty(assetPaginationDTO.getParams().getLibelle())) {

			predicate.and(
					qAsset.libelle.like("%" + assetPaginationDTO.getParams().getLibelle() + "%"));
		}

		// find LIKE identity
		if (!ACMValidationUtils.isNullOrEmpty(assetPaginationDTO.getParams().getDescription())) {
			predicate.and(qAsset.description
					.like("%" + assetPaginationDTO.getParams().getDescription() + "%"));
		}
		// find LIKE supplier
		if (!ACMValidationUtils.isNullOrEmpty(assetPaginationDTO.getParams().getSupplierName())) {
			predicate.and(qAsset.supplierName
					.like("%" + assetPaginationDTO.getParams().getSupplierName() + "%"));
		}
		// find by supplier status
		if (!ACMValidationUtils.isNullOrEmpty(assetPaginationDTO.getParams().getSupplier())
				&& !ACMValidationUtils
						.isNullOrEmpty(assetPaginationDTO.getParams().getSupplier().getStatus())) {
			predicate.and(qAsset.supplier.status
					.eq(assetPaginationDTO.getParams().getSupplier().getStatus()));
		}

		if (!ACMValidationUtils.isNullOrEmpty(assetPaginationDTO.getParams().getSupplier())
				&& !ACMValidationUtils.isNullOrEmpty(
						assetPaginationDTO.getParams().getSupplier().getStatusRejected())) {
			predicate.and(qAsset.supplier.status.notEqualsIgnoreCase(
					assetPaginationDTO.getParams().getSupplier().getStatusRejected()));
		}

		// find by supplier status

		predicate.and(qAsset.supplier.status.notEqualsIgnoreCase("NON CONTRACTED"));

		// find by supplier
		if (!ACMValidationUtils.isNullOrEmpty(assetPaginationDTO.getParams().getSupplier())
				&& !ACMValidationUtils
						.isNullOrEmpty(assetPaginationDTO.getParams().getSupplier().getId())) {
			predicate.and(
					qAsset.supplier.id.eq(assetPaginationDTO.getParams().getSupplier().getId()));
		}
		// find LIKE identity
		if (!ACMValidationUtils.isNullOrEmpty(assetPaginationDTO.getParams().getPrixUnitaire())) {
			predicate.and(qAsset.prixUnitaire
					.like("%" + assetPaginationDTO.getParams().getPrixUnitaire() + "%"));
		}
		// find LIKE identity
		if (!ACMValidationUtils.isNullOrEmpty(assetPaginationDTO.getParams().getRemiseArticle())) {
			predicate.and(qAsset.remiseArticle
					.like("%" + assetPaginationDTO.getParams().getRemiseArticle() + "%"));
		}
		// find LIKE identity
		if (!ACMValidationUtils.isNullOrEmpty(assetPaginationDTO.getParams().getInsertBy())) {
			predicate.and(
					qAsset.insertBy.like("%" + assetPaginationDTO.getParams().getInsertBy() + "%"));
		}
		// find by date insertion
		if (!ACMValidationUtils.isNullOrEmpty(assetPaginationDTO.getParams().getDateInsertion())) {
			Timestamp StartTimesTamp = DateUtil
					.dateToDateTime(assetPaginationDTO.getParams().getDateInsertion(), "00:00:01");
			Timestamp EndTimesTamp = DateUtil
					.dateToDateTime(assetPaginationDTO.getParams().getDateInsertion(), "23:59:59");
			predicate.and(qAsset.dateInsertion.between(StartTimesTamp, EndTimesTamp));
		}

		// find by date insertion
		if (!ACMValidationUtils.isNullOrEmpty(assetPaginationDTO.getParams().getDateInsertion())) {
			Timestamp StartTimesTamp = DateUtil
					.dateToDateTime(assetPaginationDTO.getParams().getDateInsertion(), "00:00:01");
			Timestamp EndTimesTamp = DateUtil
					.dateToDateTime(assetPaginationDTO.getParams().getDateInsertion(), "23:59:59");
			predicate.and(qAsset.dateInsertion.between(StartTimesTamp, EndTimesTamp));
		}

		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("1".equals(assetPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(assetPaginationDTO.getSortField())) {
			// customerNameNoPipe => customerName
			String sortedField = assetPaginationDTO.getSortField();
			if (assetPaginationDTO.getSortField().equals("codeArticle")) {
				sortedField = "codeArticle";
			}
			pageable = PageRequest.of(assetPaginationDTO.getPageNumber(),
					assetPaginationDTO.getPageSize(), Sort.Direction.ASC, sortedField);
		}
		else if ("-1".equals(assetPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(assetPaginationDTO.getSortField())) {
			// customerNameNoPipe => customerName
			String sortedField = assetPaginationDTO.getSortField();
			if (assetPaginationDTO.getSortField().equals("codeArticle")) {
				sortedField = "codeArticle";
			}
			pageable = PageRequest.of(assetPaginationDTO.getPageNumber(),
					assetPaginationDTO.getPageSize(), Sort.Direction.DESC, sortedField);
		}
		else {
			// default sort by dateInsertion : DESC
			pageable = PageRequest.of(assetPaginationDTO.getPageNumber(),
					assetPaginationDTO.getPageSize(), Sort.Direction.DESC, "dateInsertion");
		}

		// load data
		Page<Asset> pagedResult = assetRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<Asset> assets = pagedResult.getContent();
			List<AssetDTO> assetDTOs = new ArrayList<>();
			// mapping data
			assets.forEach(asset -> {
				AssetDTO dto = mapper.map(asset, AssetDTO.class);

				assetDTOs.add(dto);
			});

			logger.info("{} : Customer was founded (PageNumber = {} / PageSize = {} )",
					assets.size(), assetPaginationDTO.getPageNumber(),
					assetPaginationDTO.getPageSize());
			// setting data
			assetPaginationDTO.setResultsAssets(assetDTOs);
			// setting totals pages
			assetPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			assetPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		return assetPaginationDTO;
	}

}
