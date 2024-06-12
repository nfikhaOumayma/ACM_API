/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.client.CreditClient;
import com.acm.client.ReportingClient;
import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.type.ProductQueryExepction;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ProductRepository;
import com.acm.service.ProductDetailsService;
import com.acm.service.ProductService;
import com.acm.service.SettingDocumentProductService;
import com.acm.service.SettingDocumentTypeService;
import com.acm.service.SettingLevelProcessService;
import com.acm.service.SettingLevelService;
import com.acm.service.SettingListValuesService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.DeferredPeriodTypeDTO;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.ProductDetailsDTO;
import com.acm.utils.dtos.SettingDocumentProductDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.dtos.SettingLevelDTO;
import com.acm.utils.dtos.SettingLevelProcessDTO;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.models.Product;
import com.acm.utils.models.QProduct;
import com.acm.utils.string.StringUtils;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

import feign.FeignException;

/**
 * {@link ProductServiceImpl} ProductServiceImpl.
 * 
 * @author HaythemBenizid
 * @since 1.0.2
 */
@Service
public class ProductServiceImpl implements ProductService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(ProductServiceImpl.class);

	/** The product repository. */
	@Autowired
	private ProductRepository productRepository;

	/** The setting level service. */
	@Autowired
	private SettingLevelService settingLevelService;

	/** The setting level process service. */
	@Autowired
	private SettingLevelProcessService settingLevelProcessService;

	/** The setting document type service. */
	@Autowired
	private SettingDocumentTypeService settingDocumentTypeService;

	/** The setting document product service. */
	@Autowired
	private SettingDocumentProductService settingDocumentProductService;

	/** The product details service. */
	@Autowired
	private ProductDetailsService productDetailsService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The mail sender client. */
	@Autowired
	private ReportingClient mailSenderClient;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The default ACM receiver mail. */
	@Autowired
	private String defaultACMReceiverMail;

	/** The setting list values service. */
	@Autowired
	private SettingListValuesService settingListValuesService;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductService#find(java.lang.Long)
	 */
	@Override
	public ProductDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Product by ID : {}", id);
		Product product = productRepository.findById(id).orElse(null);
		// init productDTO
		ProductDTO productDTO = mapper.map(product, ProductDTO.class);
		// set list of DeferredPeriodTypeDTO for each productDetails
		List<DeferredPeriodTypeDTO> deferredPeriodTypeDTOs =
				settingListValuesService.findDeferredPeriodTypes();
		// set DeferredPeriodTypeDTOs for each product detail of the found product
		for (ProductDetailsDTO productDetailsDTO : productDTO.getProductDetailsDTOs()) {
			List<String> deferredTypeIds = new ArrayList<>(Arrays.asList(
					StringUtils.convertingString(productDetailsDTO.getDeferredPeriodTypes(), ",")));
			productDetailsDTO.setDeferredPeriodTypeDTOs(deferredPeriodTypeDTOs.stream().filter(
					para -> deferredTypeIds.contains(para.getDeferredPeriodTypeId().toString()))
					.collect(Collectors.toList()));
		}

		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(product)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Product.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ Product.class.getSimpleName() + CommonExceptionsMessage.WITH_ID + id);
		}
		return productDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductService#find(com.acm.utils.dtos.ProductDTO)
	 */
	@Override
	public List<ProductDTO> find(ProductDTO productDTO) {

		Preconditions.checkNotNull(productDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// init QProduct
		QProduct qProduct = QProduct.product;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qProduct.enabled.eq(Boolean.TRUE));
		// find by id product Abacus
		if (!ACMValidationUtils.isNullOrEmpty(productDTO.getProductIdAbacus())) {
			predicate.and(qProduct.productIdAbacus.eq(productDTO.getProductIdAbacus()));
		}
		// find by productGrp

		if (!ACMValidationUtils.isNullOrEmpty(productDTO.getProductGrp())
				&& Boolean.TRUE.equals(productDTO.getProductGrp())) {
			predicate.and(qProduct.productGrp.eq(productDTO.getProductGrp()));
		}
		// find by productIndiv
		if (!ACMValidationUtils.isNullOrEmpty(productDTO.getProductIndiv())
				&& Boolean.TRUE.equals(productDTO.getProductIndiv())) {
			predicate.and(qProduct.productIndiv.eq(productDTO.getProductIndiv()));
		}
		// find by productOrg
		if (!ACMValidationUtils.isNullOrEmpty(productDTO.getProductOrg())
				&& Boolean.TRUE.equals(productDTO.getProductOrg())) {
			predicate.and(qProduct.productOrg.eq(productDTO.getProductOrg()));
		}

		// filter by rate start date > = current date
		predicate.and(qProduct.rateEndDate.goe(DateUtil.setCurrentTimeToDate(new Date())));

		Iterable<Product> iterable = productRepository.findAll(predicate);
		List<Product> products = new ArrayList<>();
		iterable.forEach(products::add);
		logger.info("{} : Product was founded", products.size());
		// set list of DeferredPeriodTypeDTO for each productDetails
		List<DeferredPeriodTypeDTO> deferredPeriodTypeDTOs =
				settingListValuesService.findDeferredPeriodTypes();
		List<ProductDTO> productsDTOs = new ArrayList<>();
		products.forEach(product -> productsDTOs.add(mapper.map(product, ProductDTO.class)));
		productsDTOs.forEach(productDTOParam -> {
			for (ProductDetailsDTO productDetailsDTO : productDTOParam.getProductDetailsDTOs()) {
				List<String> deferredTypeIds = new ArrayList<>(Arrays.asList(StringUtils
						.convertingString(productDetailsDTO.getDeferredPeriodTypes(), ",")));
				productDetailsDTO.setDeferredPeriodTypeDTOs(deferredPeriodTypeDTOs.stream().filter(
						para -> deferredTypeIds.contains(para.getDeferredPeriodTypeId().toString()))
						.collect(Collectors.toList()));
			}

		});
		return productsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductService#save(com.acm.utils.dtos.ProductDTO)
	 */
	@Override
	public ProductDTO save(ProductDTO productDTO) {

		Preconditions.checkNotNull(productDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Product product = mapper.map(productDTO, Product.class);

		CommonFunctions.mapperToSave(product, userClient, logger);
		// setting enbled
		product.setEnabled(productDTO.getEnabled());
		product.setId(product.getProductIdAbacus());
		Product newProduct = productRepository.save(product);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, Product.class.getSimpleName());
		return mapper.map(newProduct, ProductDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductService#save(java.lang.Long, com.acm.utils.dtos.ProductDTO)
	 */
	@Override
	public ProductDTO save(Long id, ProductDTO productDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(productDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update Product with ID = {}", id);
		Product oldProduct = productRepository.findById(id).orElse(null);

		// check if object is null
		if (oldProduct == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Product.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Product.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldProduct)
		mapper.map(productDTO, oldProduct);
		CommonFunctions.mapperToUpdate(oldProduct, userClient, logger);
		// setting enbled
		oldProduct.setEnabled(productDTO.getEnabled());

		// update & persist data in DB
		Product newProduct = productRepository.save(oldProduct);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Product.class.getSimpleName());
		return mapper.map(newProduct, ProductDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductService#processByBatch(com.acm.utils.dtos.ProductDTO)
	 */
	@Override
	public ProductDTO processByBatch(ProductDTO productDTO, String token)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(productDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(productDTO.getCode(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.debug("processing Product = {}", productDTO);
		// find product by Code
		List<Product> products = productRepository.findByCode(productDTO.getCode());
		if (ACMValidationUtils.isNullOrEmpty(products)) {
			productDTO.setIssueFeePercentage1(productDTO.getIssueFeePercentage1() != null
					? productDTO.getIssueFeePercentage1()
					: BigDecimal.ZERO);
			productDTO.setIssueFeePercentage2(productDTO.getIssueFeePercentage2() != null
					? productDTO.getIssueFeePercentage2()
					: BigDecimal.ZERO);
			productDTO.setIssueFeePercentage3(productDTO.getIssueFeePercentage3() != null
					? productDTO.getIssueFeePercentage3()
					: BigDecimal.ZERO);
			productDTO.setIssueFeePercentage4(productDTO.getIssueFeePercentage4() != null
					? productDTO.getIssueFeePercentage4()
					: BigDecimal.ZERO);
			productDTO.setIssueFeeVAT1(
					productDTO.getIssueFeeVAT1() != null ? productDTO.getIssueFeeVAT1()
							: BigDecimal.ZERO);
			productDTO.setIssueFeeVAT2(
					productDTO.getIssueFeeVAT2() != null ? productDTO.getIssueFeeVAT2()
							: BigDecimal.ZERO);
			productDTO.setInsuranceVat(
					productDTO.getInsuranceVat() != null ? productDTO.getInsuranceVat()
							: BigDecimal.ZERO);

			// setting round type
			productDTO.setRoundType(
					productDTO.getRoundType() != null ? productDTO.getRoundType() : "");
			// setting issue fee fix
			productDTO.setIssueFeeAmount1(
					productDTO.getIssueFeeAmount1() != null ? productDTO.getIssueFeeAmount1()
							: BigDecimal.ZERO);
			productDTO.setIssueFeeAmount2(
					productDTO.getIssueFeeAmount2() != null ? productDTO.getIssueFeeAmount2()
							: BigDecimal.ZERO);
			// setting flat interest rate
			productDTO.setFlatInterestRate(
					productDTO.getFlatInterestRate() != null ? productDTO.getFlatInterestRate()
							: BigDecimal.ZERO);

			// setting product type
			productDTO
					.setProductGrp(productDTO.getCustomerType().contains(CustomerType.GRP.name()));
			productDTO.setProductIndiv(
					productDTO.getCustomerType().contains(CustomerType.INDIV.name()));
			productDTO
					.setProductOrg(productDTO.getCustomerType().contains(CustomerType.ORG.name()));

			// add new product to ACM DB
			ProductDTO newProductDTO = save(productDTO);
			logger.debug("newProductDTO = {} :: DONE", newProductDTO);

			/*
			 * Save LEVEL_PROCESS
			 */
			List<SettingLevelDTO> settingLevelDTOs =
					settingLevelService.find(new SettingLevelDTO());
			// save product config
			for (SettingLevelDTO settingLevelDTO : settingLevelDTOs) {
				settingLevelProcessService.save(new SettingLevelProcessDTO(newProductDTO.getId(),
						BigDecimal.ZERO, "To be configured...", settingLevelDTO));
			}

			/*
			 * Save SETTING_DOC_PRODUCT
			 */
			List<SettingDocumentTypeDTO> settingDocumentTypeDTOs =
					settingDocumentTypeService.find(new SettingDocumentTypeDTO());
			// save product config
			for (SettingDocumentTypeDTO settingDocumentTypeDTO : settingDocumentTypeDTOs) {
				settingDocumentProductService.save(new SettingDocumentProductDTO(
						settingDocumentTypeDTO, newProductDTO.getId().intValue(), Boolean.FALSE,
						new Date(), "NONE"));
			}

			// send mail notification
			sendMail(newProductDTO, token);
			return newProductDTO;
		}
		else {
			// updating existing product data with data loaded from ABACUS DB
			ProductDTO oldProductDTO = mapper.map(products.get(0), ProductDTO.class);
			oldProductDTO.setDescription(productDTO.getDescription());
			oldProductDTO.setProductIdAbacus(productDTO.getProductIdAbacus());
			oldProductDTO.setProductTypeAbacus(productDTO.getProductTypeAbacus());
			oldProductDTO.setCreationDateAbacus(productDTO.getCreationDateAbacus());
			oldProductDTO.setEditDateAbacus(productDTO.getEditDateAbacus());
			oldProductDTO.setEnabled(productDTO.getEnabled());
			oldProductDTO.setRate(productDTO.getRate());
			oldProductDTO.setRateStartDate(productDTO.getRateStartDate());
			oldProductDTO.setRateEndDate(productDTO.getRateEndDate());
			oldProductDTO.setMaximumBalance(productDTO.getMaximumBalance());
			oldProductDTO.setMaximumTerm(productDTO.getMaximumTerm());
			oldProductDTO.setMinimumTerm(productDTO.getMinimumTerm());
			oldProductDTO.setIssueFeePercentage1(productDTO.getIssueFeePercentage1() != null
					? productDTO.getIssueFeePercentage1()
					: BigDecimal.ZERO);
			oldProductDTO.setIssueFeePercentage2(productDTO.getIssueFeePercentage2() != null
					? productDTO.getIssueFeePercentage2()
					: BigDecimal.ZERO);
			oldProductDTO.setIssueFeePercentage3(productDTO.getIssueFeePercentage3() != null
					? productDTO.getIssueFeePercentage3()
					: BigDecimal.ZERO);
			oldProductDTO.setIssueFeePercentage4(productDTO.getIssueFeePercentage4() != null
					? productDTO.getIssueFeePercentage4()
					: BigDecimal.ZERO);
			oldProductDTO.setUseScheduleInterest(productDTO.getUseScheduleInterest());
			oldProductDTO.setCapitaliseInterestWhenRefinancing(
					productDTO.getCapitaliseInterestWhenRefinancing());
			oldProductDTO.setCurrency(productDTO.getCurrency());
			oldProductDTO.setDecimal(productDTO.getDecimal());
			oldProductDTO.setMaximumAge(productDTO.getMaximumAge());
			oldProductDTO.setMinimumAge(productDTO.getMinimumAge());
			oldProductDTO.setMaxAccounts(productDTO.getMaxAccounts());
			oldProductDTO.setMaximumDeferredPeriod(productDTO.getMaximumDeferredPeriod());
			oldProductDTO.setMinimumDeferredPeriod(productDTO.getMinimumDeferredPeriod());
			oldProductDTO.setCuInsuranceID(productDTO.getCuInsuranceID());
			oldProductDTO.setIssueFeeVAT1(
					productDTO.getIssueFeeVAT1() != null ? productDTO.getIssueFeeVAT1()
							: BigDecimal.ZERO);
			oldProductDTO.setIssueFeeVAT2(
					productDTO.getIssueFeeVAT2() != null ? productDTO.getIssueFeeVAT2()
							: BigDecimal.ZERO);

			oldProductDTO.setInsuranceVat(
					productDTO.getInsuranceVat() != null ? productDTO.getInsuranceVat()
							: BigDecimal.ZERO);
			// setting issue fee fix
			oldProductDTO.setIssueFeeAmount1(
					productDTO.getIssueFeeAmount1() != null ? productDTO.getIssueFeeAmount1()
							: BigDecimal.ZERO);
			oldProductDTO.setIssueFeeAmount2(
					productDTO.getIssueFeeAmount2() != null ? productDTO.getIssueFeeAmount2()
							: BigDecimal.ZERO);
			oldProductDTO.setFlatInterestRate(
					productDTO.getFlatInterestRate() != null ? productDTO.getFlatInterestRate()
							: BigDecimal.ZERO);
			// setting product type
			oldProductDTO
					.setProductGrp(productDTO.getCustomerType().contains(CustomerType.GRP.name()));
			oldProductDTO.setProductIndiv(
					productDTO.getCustomerType().contains(CustomerType.INDIV.name()));
			oldProductDTO
					.setProductOrg(productDTO.getCustomerType().contains(CustomerType.ORG.name()));
			// setting round type
			oldProductDTO.setRoundType(
					productDTO.getRoundType() != null ? productDTO.getRoundType() : "");
			// update product
			ProductDTO newProductDTO = save(oldProductDTO.getId(), oldProductDTO);
			logger.debug("newProductDTO = {} :: DONE", newProductDTO);
			return newProductDTO;
		}
	}

	/**
	 * Send mail.
	 * 
	 * @author HaythemBenizid
	 * @param productDTO the products dto
	 * @param token the token
	 */
	private void sendMail(ProductDTO productDTO, String token) {

		try {
			mailSenderClient.sendMail(new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
					defaultACMReceiverMail, "Processing PRODUCTS DATA from ABACUS-DB :: DONE",
					"Add new PRODUCT with CODE  : [" + productDTO.getCode() + "] successfully."),
					token);
		}
		catch (FeignException e) {
			logger.error("Failed to send Mail");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
		}
		logger.info("Sending Email Notification :: DONE");
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductService#find()
	 */
	@Override
	public List<ProductDTO> find() {

		List<Product> products = productRepository.findAll();
		List<ProductDTO> productsDTOs = new ArrayList<>();
		products.forEach(product -> productsDTOs.add(mapper.map(product, ProductDTO.class)));
		return productsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductService#loadSettingFromAbacus()
	 */
	@Override
	public String loadSettingFromAbacus() throws ResourcesNotFoundException, ProductQueryExepction {

		logger.info("############ Load/Refresh Setting Product From Abacus :: START ");
		// Load product from ABACUS
		List<ProductDTO> productsAbacus = transversClient.findProducts();
		// if there is no product or there is in issue when executing the query to get product from
		// abacus
		if (productsAbacus.size() == 0) {
			throw new ProductQueryExepction(
					new ExceptionResponseMessage(
							CommonErrorCode.ERROR_QUERY_GET_PRODUCT_FROM_ABACUS,
							CommonExceptionsMessage.ERROR_GET_PRODUCT_FROM_ABACUS),
					CommonExceptionsMessage.ERROR_GET_PRODUCT_FROM_ABACUS);
		}
		List<String> productsAbacusCode = new ArrayList<>();
		List<ProductDTO> productDTOs = find();
		for (ProductDTO productDTOAbacus : productsAbacus) {
			ProductDTO productDTO = productDTOs.stream()
					.filter(product -> productDTOAbacus.getCode().equals(product.getCode()))
					.findAny().orElse(null);
			java.util.Date date = new java.util.Date();
			if (((productDTO != null)
					&& ((productDTO.getFlatInterestRate() != productDTOAbacus.getFlatInterestRate())
							|| (productDTO.getRate() != productDTOAbacus.getRate()))
					&& (productDTOAbacus.getRateStartDate().after(date)))
					|| (productDTOAbacus.getRateStartDate().after(date) && productDTO == null)) {
				return "The new interest rate of the product " + productDTO.getDescription()
						+ " starts from " + productDTOAbacus.getRateStartDate()
						+ ", please retry the refresh starting from this date";
			}
		}
		// Save or Update product
		for (ProductDTO productDTO : productsAbacus) {
			if (productDTO != null && productDTO.getProductIdAbacus() != null) {
				// insert imported product into ACM DB
				ProductDTO newProductDTO = processByBatch(productDTO, productDTO.getToken());
				logger.debug("Processing Product : {} DONE", newProductDTO.getCode());

				// only add new row in productDetails for enabled product
				if (Boolean.TRUE.equals(newProductDTO.getEnabled())) {
					// find if product has any ACM_PRODUCT_DETAILS
					ProductDetailsDTO params = new ProductDetailsDTO();
					params.setIdProduct(newProductDTO.getId());
					List<ProductDetailsDTO> productDetailsDTOs = productDetailsService.find(params);
					// check is there any configuration for given product
					if (ACMValidationUtils.isNullOrEmpty(productDetailsDTOs)) {
						// Insert new row with
						// AMT_MIN = 0
						// AMT_MAX = product.getMaximumBalance
						// MIN_TERM = product.getMinimumTerm
						// MAX_TERM = product.getMaximumTerm

						// SAVE ProductDetails
						ProductDetailsDTO newProductDetailsDTO = productDetailsService
								.save(new ProductDetailsDTO(newProductDTO.getId(), 0L,
										newProductDTO.getMaximumBalance().longValue(),
										newProductDTO.getMinimumTerm(),
										newProductDTO.getMaximumTerm(), "Months"));
						logger.debug("New Product Details = {} :: DONE", newProductDetailsDTO);
					}
				}
				// init list Product_Code
				productsAbacusCode.add(productDTO.getCode());
			}
		}
		logger.info("############ Load/Refresh Setting for {} Product From Abacus :: DONE",
				productsAbacus.size());

		// Disable old Products in ACM-DB IF NOT EXISTING IN ABACUS-DB
		for (ProductDTO productDTO : productDTOs) {
			// if products dosn't exist in ABACUS list => DISABLE
			if (!(productsAbacusCode.contains(productDTO.getCode()))) {
				productDTO.setEnabled(Boolean.FALSE);
				save(productDTO.getId(), productDTO);
				logger.debug("Product with code = {} :: DISABLED ", productDTO.getCode());
			}
		}

		// find enabled products
		ProductDTO productDTO = new ProductDTO();
		productDTO.setEnabled(Boolean.TRUE);
		List<ProductDTO> productAcm = find(productDTO);
		String result = creditClient.syncIBSettingFromACM(productAcm);
		if (!ACMValidationUtils.isNullOrEmpty(result)) {
			logger.error(result);
		}
		else {
			logger.info("Sync in IB DONE");
		}

		return "Reset Product DONE";

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductService#findAll(com.acm.utils.dtos.ProductDTO)
	 */
	@Override
	public List<ProductDTO> findAll(ProductDTO productDTO) {

		Preconditions.checkNotNull(productDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// init QProduct
		QProduct qProduct = QProduct.product;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by productGrp

		if (!ACMValidationUtils.isNullOrEmpty(productDTO.getProductGrp())
				&& Boolean.TRUE.equals(productDTO.getProductGrp())) {
			predicate.and(qProduct.productGrp.eq(productDTO.getProductGrp()));
		}
		// find by productIndiv
		if (!ACMValidationUtils.isNullOrEmpty(productDTO.getProductIndiv())
				&& Boolean.TRUE.equals(productDTO.getProductIndiv())) {
			predicate.and(qProduct.productIndiv.eq(productDTO.getProductIndiv()));
		}
		// find by productOrg
		if (!ACMValidationUtils.isNullOrEmpty(productDTO.getProductOrg())
				&& Boolean.TRUE.equals(productDTO.getProductOrg())) {
			predicate.and(qProduct.productOrg.eq(productDTO.getProductOrg()));
		}

		// filter by rate start date > = current date
		predicate.and(qProduct.rateEndDate.goe(DateUtil.setCurrentTimeToDate(new Date())));

		Iterable<Product> iterable = productRepository.findAll(predicate);
		List<Product> products = new ArrayList<>();
		iterable.forEach(products::add);
		logger.info("{} : Product was founded", products.size());

		List<ProductDTO> productsDTOs = new ArrayList<>();
		products.forEach(product -> productsDTOs.add(mapper.map(product, ProductDTO.class)));
		return productsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductService#findByIds(java.lang.Long[])
	 */
	@Override
	public List<ProductDTO> findByIds(List<Long> ids) {

		Preconditions.checkNotNull(ids, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		List<ProductDTO> productDtos = new ArrayList<>();
		if (!ACMValidationUtils.isNullOrEmpty(ids)) {
			logger.info("find products by list of ids");
			List<Product> products = productRepository.findByProductIdAbacusIn(ids);
			logger.info("{} : Product was founded", products.size());
			products.forEach(product -> productDtos.add(mapper.map(product, ProductDTO.class)));
		}
		return productDtos;
	}

}
