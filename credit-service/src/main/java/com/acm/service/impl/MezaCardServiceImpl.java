/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.Query;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.acm.client.CreditClient;
import com.acm.client.ParametrageClient;
import com.acm.client.ReportingClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.MezaCardsExistInDbException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.exceptions.type.SaveFileException;
import com.acm.repository.AcmMezaCardRepository;
import com.acm.service.MezaCardService;
import com.acm.utils.dtos.AcmMezaCardDTO;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.BrancheDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.MailCustomerDTO;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.dtos.pagination.AcmMezaCardPaginationDTO;
import com.acm.utils.enums.CustomerMezaCardStatus;
import com.acm.utils.enums.MailBuilderMethod;
import com.acm.utils.enums.MezaCardStatus;
import com.acm.utils.enums.NotificationCategory;
import com.acm.utils.enums.NotificationType;
import com.acm.utils.models.AcmMezaCard;
import com.acm.utils.models.Customer;
import com.acm.utils.models.Product;
import com.acm.utils.models.QAcmMezaCard;
import com.acm.utils.string.StringUtils;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

import feign.FeignException;

/**
 * {@link MezaCardServiceImpl} class.
 *
 * @author YesserSomai
 * @since 1.0.6
 */
@Service
public class MezaCardServiceImpl implements MezaCardService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(MezaCardServiceImpl.class);

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The acm meza card repository. */
	@Autowired
	private AcmMezaCardRepository acmMezaCardRepository;

	/** The entity manager. */
	@Autowired
	private EntityManager entityManager;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The mail sender client. */
	@Autowired
	private ReportingClient mailSenderClient;

	/** The default ACM receiver mail. */
	@Autowired
	private String defaultACMReceiverMail;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmMezaCardService#find(java.lang.Long)
	 */
	@Override
	public AcmMezaCardDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find AcmMezaCard by ID : {}", id);
		AcmMezaCard acmMezaCard = acmMezaCardRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(acmMezaCard)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, AcmMezaCard.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ AcmMezaCard.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ id);
		}
		return mapper.map(acmMezaCard, AcmMezaCardDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.MezaCardService#uploadAmlFile(org.springframework.web.multipart.MultipartFile
	 * [])
	 */
	@Override
	public void uploadFile(MultipartFile[] uploadedFiles, String branch, Boolean activate)
			throws SaveFileException, JsonParseException, JsonMappingException, IOException {

		Preconditions.checkNotNull(uploadedFiles, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// convert JSON String to list of object
		ObjectMapper objectMapper = new ObjectMapper();
		BrancheDTO branchDTO = objectMapper.readValue(branch, BrancheDTO.class);
		// convert MultipartFile to File
		File file = null;
		// Init exception message if mezaCards inserted already exist in DB
		StringBuilder messageBody = new StringBuilder();
		if (uploadedFiles.length > 0) {
			for (MultipartFile multipartFile : uploadedFiles) {
				file = CommonFunctions.fileConverter(multipartFile,
						environment.getProperty("spring.servlet.multipart.location"));
			}
		}

		try {
			FileInputStream fileInputStream = new FileInputStream(file);
			// we create an XSSF Workbook object for our XLSX Excel File
			@SuppressWarnings("resource")
			XSSFWorkbook workbook = new XSSFWorkbook(fileInputStream);
			// we get first sheet
			XSSFSheet sheet = workbook != null ? workbook.getSheetAt(0) : null;
			// we iterate on rows
			Iterator<Row> rowIt = sheet != null ? sheet.iterator() : null;
			// init list
			List<AcmMezaCard> acmMezaCards = new ArrayList<>();
			int countRow = 1;
			boolean newCards = false;
			if (rowIt != null) {
				Row row = rowIt.next();
				while (rowIt != null && rowIt.hasNext()) {
					row = rowIt.next();
					// iterate on cells for the current row
					Iterator<Cell> cellIterator = row.cellIterator(); // iterating over each column
					AcmMezaCard acmMezaCard = new AcmMezaCard();
					while (cellIterator != null && cellIterator.hasNext()) {
						Cell cell = cellIterator.next();
						String cellValue = parseCellValue(cell);
						if (cell != null && !ACMValidationUtils.isNullOrEmpty(cellValue)) {
							// MERCHANT ID
							if (cell.getColumnIndex() == 0) {
								acmMezaCard.setMerchantID(new BigDecimal(cellValue));
							}
							// CARD TYPE
							if (cell.getColumnIndex() == 1) {
								acmMezaCard.setCardType(
										String.valueOf(new BigDecimal(cellValue).toBigInteger()));
							}
							// CARD NUMBER
							if (cell.getColumnIndex() == 2) {
								acmMezaCard.setCardNumber(cellValue);
							}
							// ACCOUNT
							if (cell.getColumnIndex() == 3) {
								acmMezaCard.setAccount(
										String.valueOf(new BigDecimal(cellValue).toBigInteger()));
							}
							// EXPIRTY DATE
							if (cell.getColumnIndex() == 4) {
								SimpleDateFormat simpleDateFormat =
										new SimpleDateFormat(CommonConstants.PATTREN_DATE);
								acmMezaCard.setExpirtyDate(simpleDateFormat.parse(cellValue));
							}
							// ACTIVITY DATE
							if (cell.getColumnIndex() == 5) {
								SimpleDateFormat simpleDateFormat =
										new SimpleDateFormat(CommonConstants.PATTREN_DATE);
								acmMezaCard.setActivityDate(simpleDateFormat.parse(cellValue));
							}
							// ACTIVITY DATE
							if (cell.getColumnIndex() == 6) {
								acmMezaCard.setEmbossedName(cellValue);
							}
						}
					}
					countRow++;
					// check && save card
					if (!ACMValidationUtils.isNullOrEmpty(acmMezaCard.getMerchantID())
							&& !ACMValidationUtils.isNullOrEmpty(acmMezaCard.getAccount())
							&& !ACMValidationUtils.isNullOrEmpty(acmMezaCard.getCardNumber())
							&& !ACMValidationUtils.isNullOrEmpty(acmMezaCard.getCardType())) {
						if (activate.equals(Boolean.TRUE)
								&& !ACMValidationUtils.isNullOrEmpty(branchDTO)) {

							acmMezaCard.setStatus(MezaCardStatus.SENT.name());
							acmMezaCard.setBranchID(branchDTO.getBranchID());
							acmMezaCard.setBranchName(branchDTO.getName());
						}
						else {
							// set default status = UPLOAD
							acmMezaCard.setStatus(MezaCardStatus.UPLOAD.name());
						}
						// check MEZA card if exist
						AcmMezaCard existAcmMezaCard =
								acmMezaCardRepository.findFirstByMerchantIDAndCardNumber(
										acmMezaCard.getMerchantID(), acmMezaCard.getCardNumber());
						if (ACMValidationUtils.isNullOrEmpty(existAcmMezaCard)) {
							CommonFunctions.mapperToSave(acmMezaCard, userClient, logger);
							// save new card
							acmMezaCards.add(acmMezaCardRepository.save(acmMezaCard));
							newCards = true;
						}
						else {
							messageBody.append(acmMezaCard.getCardNumber() + " , ");
						}
					}
				}
			}
			logger.info("countRow = {} / acmMezaCards size = {} ", countRow, acmMezaCards.size());
			// if there is Meza Cards already exist in DB then send it To FrontEnd in an exception
			if (!messageBody.toString().equals("") && !newCards) {
				throw new MezaCardsExistInDbException(
						new ExceptionResponseMessage(CommonErrorCode.CARDS_EXIST_IN_DATABASE,
								CommonExceptionsMessage.CARDS_EXIST_IN_DATABASE,
								new TechnicalException()),
						CommonExceptionsMessage.CARDS_EXIST_IN_DATABASE);
			}
		}
		catch (Exception e) {
			throw new SaveFileException(CommonExceptionsMessage.SAVE_FILE_EXECPTION,
					e.getMessage());
		}
	}

	/**
	 * Parses the cell value.
	 * 
	 * @author YesserSomai
	 * @param cell the cell
	 * @return the string
	 */
	private String parseCellValue(Cell cell) {

		if (cell != null) {
			// field that represents string cell type
			if (cell.getCellType().name().equals(CellType.STRING.name())) {
				return cell.getStringCellValue();
			}
			// field that represents number cell type
			else if (cell.getCellType().name().equals(CellType.NUMERIC.name())) {
				String cellValue = String.valueOf(cell.getNumericCellValue());
				if (DateUtil.isCellDateFormatted(cell)) {
					DateFormat df = new SimpleDateFormat(CommonConstants.PATTREN_DATE);
					Date date = cell.getDateCellValue();
					cellValue = df.format(date);
				}
				return cellValue;
			}
			// field that represents BOOLEAN cell type
			else if (cell.getCellType().name().equals(CellType.BOOLEAN.name())) {
				return String.valueOf(cell.getBooleanCellValue());
			}
			// field that represents Formula cell type
			else if (cell.getCellType().name().equals(CellType.FORMULA.name())) {
				return cell.getCellFormula();
			}
			// field that represents BLANK cell type
			else if (cell.getCellType().name().equals(CellType.BLANK.name())) {
				return "";
			}
			else {
				return "";
			}
		}
		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.MezaCardService#find(com.acm.utils.dtos.pagination.AcmMezaCardPaginationDTO)
	 */
	@Override
	public AcmMezaCardPaginationDTO find(AcmMezaCardPaginationDTO acmMezaCardPaginationDTO) {

		Preconditions.checkNotNull(acmMezaCardPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(acmMezaCardPaginationDTO.getPageNumber())) {
			acmMezaCardPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(acmMezaCardPaginationDTO.getPageSize())) {
			acmMezaCardPaginationDTO.setPageSize(10);
		}
		// setting default data
		acmMezaCardPaginationDTO.setResultsAcmMezaCards(new ArrayList<>());
		// setting default totals pages
		acmMezaCardPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		acmMezaCardPaginationDTO.setTotalPages(0);
		// init QAcmMezaCard
		QAcmMezaCard qAcmMezaCard = QAcmMezaCard.acmMezaCard;
		// build Predicate using given params
		BooleanBuilder predicate = buildQuery(acmMezaCardPaginationDTO.getParams(), qAcmMezaCard);

		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("1".equals(acmMezaCardPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(acmMezaCardPaginationDTO.getSortField())) {
			pageable = PageRequest.of(acmMezaCardPaginationDTO.getPageNumber(),
					acmMezaCardPaginationDTO.getPageSize(), Sort.Direction.ASC,
					acmMezaCardPaginationDTO.getSortField());
		}
		else if ("-1".equals(acmMezaCardPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(acmMezaCardPaginationDTO.getSortField())) {
			pageable = PageRequest.of(acmMezaCardPaginationDTO.getPageNumber(),
					acmMezaCardPaginationDTO.getPageSize(), Sort.Direction.DESC,
					acmMezaCardPaginationDTO.getSortField());
		}
		else {
			// default sort by code : ASC
			pageable = PageRequest.of(acmMezaCardPaginationDTO.getPageNumber(),
					acmMezaCardPaginationDTO.getPageSize(), Sort.Direction.ASC, "merchantID");
		}

		// load data
		Page<AcmMezaCard> pagedResult = acmMezaCardRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<AcmMezaCard> acmMezaCards = pagedResult.getContent();
			logger.info("{} : AcmMezaCard was founded (PageNumber = {} / PageSize = {} )",
					acmMezaCards.size(), acmMezaCardPaginationDTO.getPageNumber(),
					acmMezaCardPaginationDTO.getPageSize());
			List<AcmMezaCardDTO> acmMezaCardDTOs = new ArrayList<>();
			acmMezaCards
					.forEach(card -> acmMezaCardDTOs.add(mapper.map(card, AcmMezaCardDTO.class)));
			// setting data
			acmMezaCardPaginationDTO.setResultsAcmMezaCards(acmMezaCardDTOs);
			// setting totals pages
			acmMezaCardPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			acmMezaCardPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		return acmMezaCardPaginationDTO;
	}

	/**
	 * Builds the query.
	 *
	 * @author YesserSomai
	 * @param acmMezaCardDTO the acm meza card DTO
	 * @param qAcmMezaCard the q acm meza card
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(AcmMezaCardDTO acmMezaCardDTO, QAcmMezaCard qAcmMezaCard) {

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// MezaCard.merchantID
		if (!ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getMerchantID())) {
			predicate.and(qAcmMezaCard.merchantID.eq(acmMezaCardDTO.getMerchantID()));
		}
		// MezaCard.cardType
		if (!ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getCardType())) {
			predicate.and(qAcmMezaCard.cardType.like('%' + acmMezaCardDTO.getCardType() + '%'));
		}
		// MezaCard.cardNumber
		if (!ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getCardNumber())) {
			predicate.and(qAcmMezaCard.cardNumber.like('%' + acmMezaCardDTO.getCardNumber() + '%'));
		}
		// MezaCard.account
		if (!ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getAccount())) {
			predicate.and(qAcmMezaCard.account.like('%' + acmMezaCardDTO.getAccount() + '%'));
		}
		// MezaCard.expirtyDate
		if (!ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getExpirtyDate())) {
			predicate.and(qAcmMezaCard.expirtyDate.eq(acmMezaCardDTO.getExpirtyDate()));
		}
		// MezaCard.activityDate
		if (!ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getActivityDate())) {
			predicate.and(qAcmMezaCard.activityDate.eq(acmMezaCardDTO.getActivityDate()));
		}
		// MezaCard.embossedName
		if (!ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getEmbossedName())) {
			predicate.and(
					qAcmMezaCard.embossedName.like('%' + acmMezaCardDTO.getEmbossedName() + '%'));
		}
		// Find by Access Branches for User Connected
		if (!ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getAccessBranch())
				&& Boolean.TRUE.equals(acmMezaCardDTO.getAccessBranch())) {
			// find User Connected
			UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
			// Create List of Access Branches
			int[] arrayBranchIds = Arrays.asList(userDTO.getAccessBranches().split(",")).stream()
					.map(String::trim).mapToInt(Integer::parseInt).toArray();
			List<Long> listBranchIds = new ArrayList<>(arrayBranchIds.length);
			for (int i : arrayBranchIds) {
				listBranchIds.add(Long.valueOf(i));
			}
			// MezaCard.branchID
			predicate.and(qAcmMezaCard.branchID.in(listBranchIds));
		}
		else {
			// MezaCard.branchID
			if (!ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getBranchID())) {
				predicate.and(qAcmMezaCard.branchID.eq(acmMezaCardDTO.getBranchID()));
			}
			// MezaCard.branchName
			if (!ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getBranchName())) {
				predicate.and(qAcmMezaCard.branchName.eq(acmMezaCardDTO.getBranchName()));
			}
		}
		// MezaCard.status
		if (!ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getListStatus())) {
			predicate.and(qAcmMezaCard.status.in(acmMezaCardDTO.getListStatus()));
		}
		// MezaCard.customerID
		if (!ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getCustomerDTO())
				&& !ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getCustomerDTO().getId())) {
			predicate.and(qAcmMezaCard.customer.id.eq(acmMezaCardDTO.getCustomerDTO().getId()));
		}
		return predicate;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.MezaCardService#save(java.util.List)
	 */
	@Override
	public List<AcmMezaCardDTO> save(List<AcmMezaCardDTO> acmMezaCardDTOs)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(acmMezaCardDTOs, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update list of Meza cards with size = {}", acmMezaCardDTOs.size());
		List<AcmMezaCardDTO> returnMezaCardDTOs = new ArrayList<>();
		Boolean checkSentCard = Boolean.FALSE;
		for (AcmMezaCardDTO acmMezaCardDTO : acmMezaCardDTOs) {
			Preconditions.checkNotNull(acmMezaCardDTO,
					CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
			AcmMezaCard acmMezaCardOld =
					acmMezaCardRepository.findById(acmMezaCardDTO.getIdMezaCard()).orElse(null);
			// check if object is null
			if (acmMezaCardOld == null) {
				logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Product.class.getSimpleName());
				throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
						CommonExceptionsMessage.NOT_FOUND + Product.class.getSimpleName()
								+ CommonExceptionsMessage.WITH_ID + acmMezaCardDTO.getIdMezaCard());
			}
			// mapping new data with existing data (oldProduct)
			mapper.map(acmMezaCardDTO, acmMezaCardOld);
			CommonFunctions.mapperToUpdate(acmMezaCardOld, userClient, logger);
			// update & persist data in DB
			AcmMezaCard acmMezaCardNew = acmMezaCardRepository.save(acmMezaCardOld);
			logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Product.class.getSimpleName());
			returnMezaCardDTOs.add(mapper.map(acmMezaCardNew, AcmMezaCardDTO.class));
			// check if meza card new status is SENT
			if (acmMezaCardNew.getStatus().equals(MezaCardStatus.SENT.name())) {
				checkSentCard = Boolean.TRUE;
			}
		}
		// Meza card status changed to sent then sent Mail and
		// Notification to the branch operation users of selected branch
		if (checkSentCard.equals(Boolean.TRUE)) {
			// send Email and notification by Groupe Code and Branch id
			sendMailAndNotification(acmMezaCardDTOs.get(0).getBranchID().intValue());
		}
		return returnMezaCardDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.MezaCardService#updateStatus(com.acm.utils.dtos.AcmMezaCardDTO)
	 */
	@Override
	public AcmMezaCardDTO updateStatus(AcmMezaCardDTO acmMezaCardDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(acmMezaCardDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		AcmMezaCard acmMezaCardOld =
				acmMezaCardRepository.findById(acmMezaCardDTO.getIdMezaCard()).orElse(null);
		// check if object is null
		if (acmMezaCardOld == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Product.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Product.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + acmMezaCardDTO.getIdMezaCard());
		}
		// mapping new data with existing data (oldProduct)
		acmMezaCardOld.setStatus(acmMezaCardDTO.getStatus());
		CommonFunctions.mapperToUpdate(acmMezaCardOld, userClient, logger);
		// update & persist data in DB
		AcmMezaCard acmMezaCardNew = acmMezaCardRepository.save(acmMezaCardOld);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Product.class.getSimpleName());
		// check if meza card new status is SENT

		return mapper.map(acmMezaCardNew, AcmMezaCardDTO.class);
	}

	/**
	 * Send mail and notification.
	 * 
	 * @author idridi
	 * @param branchID the branch ID
	 */
	private void sendMailAndNotification(Integer branchID) {

		// find users by group code and branch id
		List<UserDTO> userDTOs = userClient.findByGroupeCodeAndBranchID(
				CommonConstants.USER_GROUPE_BRANCH_OPERATION, branchID);
		// sent mails to branch operation users
		for (UserDTO user : userDTOs) {
			// send mail
			sendMail(new MailCustomerDTO(user, user.getLogin(), new MailDTO(
					CommonConstants.NO_REPLAY_EMAIL,
					(!ACMValidationUtils.isNullOrEmpty(user.getEmail())
							&& Boolean.TRUE.equals(StringUtils.mailIsValid(user.getEmail())))
									? user.getEmail()
									: defaultACMReceiverMail,
					"New Meza Card has been Assigned", ""),
					MailBuilderMethod.BUILD_USER_ASSIGNED_MEZA_CARD));

			// send notification
			SimpleDateFormat formatter = new SimpleDateFormat(CommonConstants.PATTREN_DATE);
			String actionDescription =
					"New Meza Card has been sent at " + formatter.format(new Date());
			NotificationsDTO notificationsDTO =
					creditClient.create(new NotificationsDTO(user.getLogin(),
							NotificationCategory.MEZA.name(), NotificationType.INFO.name(),
							Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_SENT_MEZA_CARD,
							actionDescription, null, null));
			logger.info("New assigned Notification  [{}] has been inserted.", notificationsDTO);
		}
	}

	/**
	 * Send mail.
	 * 
	 * @author idridi
	 * @param mailCustomerDTO the mail customer DTO
	 */
	private void sendMail(MailCustomerDTO mailCustomerDTO) {

		try {
			mailSenderClient.sendEmail(mailCustomerDTO);
			logger.info("Sending Email to customer :: DONE");
		}
		catch (FeignException e) {
			logger.error("Failed to send Mail");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.MezaCardService#findFirstOrderByAccount(com.acm.utils.dtos.AcmMezaCardDTO)
	 */
	@Override
	public AcmMezaCardDTO findByBranchIDAndStatus(AcmMezaCardDTO acmMezaCardDTO) {

		Preconditions.checkNotNull(acmMezaCardDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(acmMezaCardDTO.getBranchID(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(acmMezaCardDTO.getStatus(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find find By BranchID : {} And Status : {} OrderByCardNumber",
				acmMezaCardDTO.getBranchID(), acmMezaCardDTO.getStatus());

		// AcmMezaCard acmMezaCard =
		// acmMezaCardRepository.findFirstByBranchIDAndStatusOrderByCardNumber(
		// acmMezaCardDTO.getBranchID(), acmMezaCardDTO.getStatus());
		//

		AcmMezaCard acmMezaCard = null;
		List<String> acmMezaCardBanList = new ArrayList<>();
		acmMezaCardBanList.add("");
		Boolean noCard = Boolean.FALSE;
		Boolean cardExist = Boolean.FALSE;
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);

		do {
			acmMezaCard = acmMezaCardRepository
					.findFirstByBranchIDAndStatusAndCardNumberNotInOrderByCardNumber(
							acmMezaCardDTO.getBranchID(), acmMezaCardDTO.getStatus(),
							acmMezaCardBanList);
			if (!ACMValidationUtils.isNullOrEmpty(acmMezaCard)) {
				acmMezaCardBanList.add(acmMezaCard.getCardNumber());

				cardExist = parametrageClient.findAndAddIfNotExist(userDTO.getLogin(),
						acmMezaCard.getCardNumber());
			}
			else {
				noCard = Boolean.TRUE;
			}
		}
		while (!noCard && cardExist);

		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(acmMezaCard)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, AcmMezaCard.class.getSimpleName());
			return null;
		}
		logger.debug("AcmMezaCard value : {}", acmMezaCard);
		return mapper.map(acmMezaCard, AcmMezaCardDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmMezaCardService#save(java.lang.Long,
	 * com.acm.utils.dtos.AcmMezaCardDTO)
	 */
	@Override
	public AcmMezaCardDTO save(Long id, AcmMezaCardDTO acmMezaCardDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(acmMezaCardDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.debug("Update AcmMezaCard  with ID = {}", id);
		AcmMezaCard oldAcmMezaCard = acmMezaCardRepository.findById(id).orElse(null);
		// check if object is null
		if (oldAcmMezaCard == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, AcmMezaCard.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + AcmMezaCard.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldDocumentsLoan)
		mapper.map(acmMezaCardDTO, oldAcmMezaCard);
		// mappee customer
		oldAcmMezaCard.setCustomer(acmMezaCardDTO.getCustomerDTO() != null
				? new Customer(acmMezaCardDTO.getCustomerDTO().getId())
				: new Customer());
		CommonFunctions.mapperToUpdate(oldAcmMezaCard, userClient, logger);
		AcmMezaCard newAcmMezaCard = acmMezaCardRepository.save(oldAcmMezaCard);
		logger.debug(CommonLoggerMessage.SUCCESFULL_UPDATE, AcmMezaCard.class.getSimpleName());
		return mapper.map(newAcmMezaCard, AcmMezaCardDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.MezaCardService#update(com.acm.utils.dtos.AcmMezaCardDTO)
	 */
	@Override
	public AcmMezaCardDTO update(AcmMezaCardDTO acmMezaCardDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(acmMezaCardDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		AcmMezaCardDTO oldAcmMezaCardDTO = null;
		// find MEZA card data by ID or by given customer
		if (acmMezaCardDTO.getIdMezaCard() != null) {
			// USE IN CASE TO ASSIGN TO NEW CUSTOMER
			oldAcmMezaCardDTO = find(acmMezaCardDTO.getIdMezaCard());
			// update MEZA card status
			oldAcmMezaCardDTO.setStatus(acmMezaCardDTO.getStatus());
			// update id customer
			oldAcmMezaCardDTO.setCustomerDTO(acmMezaCardDTO.getCustomerDTO());
		}
		else {
			// USE IN CASE TO RE-ACTIVATE CARD
			List<AcmMezaCardDTO> acmMezaCardDTOs =
					find(new AcmMezaCardDTO(acmMezaCardDTO.getCustomerDTO()));
			if (!ACMValidationUtils.isNullOrEmpty(acmMezaCardDTOs)) {
				oldAcmMezaCardDTO = acmMezaCardDTOs.get(0);
				// update MEZA card status
				oldAcmMezaCardDTO.setStatus(acmMezaCardDTO.getStatus());
				// update id customer
				oldAcmMezaCardDTO.setCustomerDTO(null);
			}
		}
		// Processing founded data
		if (oldAcmMezaCardDTO != null) {
			if (ACMValidationUtils.isNullOrEmpty(oldAcmMezaCardDTO.getCustomerDTO())) {
				acmMezaCardRepository.deleteById(oldAcmMezaCardDTO.getIdMezaCard());
				oldAcmMezaCardDTO.setIdMezaCard(null);
				return save(oldAcmMezaCardDTO);
			}
			// update data
			return save(oldAcmMezaCardDTO.getIdMezaCard(), oldAcmMezaCardDTO);
		}
		return acmMezaCardDTO;
	}

	/**
	 * Save.
	 * 
	 * @author YesserSomai
	 * @param acmMezaCardDTO the acm meza card DTO
	 * @return the acm meza card DTO
	 */
	private AcmMezaCardDTO save(AcmMezaCardDTO acmMezaCardDTO) {

		Preconditions.checkNotNull(acmMezaCardDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		AcmMezaCard acmMezaCard = mapper.map(acmMezaCardDTO, AcmMezaCard.class);
		CommonFunctions.mapperToSave(acmMezaCard, userClient, logger);
		AcmMezaCard newAcmMezaCard = acmMezaCardRepository.save(acmMezaCard);
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AcmMezaCard.class.getSimpleName());
		return mapper.map(newAcmMezaCard, AcmMezaCardDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.MezaCardService#find(com.acm.utils.dtos.AcmMezaCardDTO)
	 */
	@Override
	public List<AcmMezaCardDTO> find(AcmMezaCardDTO acmMezaCardDTO) {

		Preconditions.checkNotNull(acmMezaCardDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		if (ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getCardNumber())
				&& ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getCustomerDTO().getId())) {
			return new ArrayList<>();
		}
		// init QAcmMezaCard
		QAcmMezaCard qAcmMezaCard = QAcmMezaCard.acmMezaCard;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qAcmMezaCard.enabled.eq(Boolean.TRUE));
		// get only Activate / Assigned card
		predicate.and(qAcmMezaCard.status.ne(MezaCardStatus.UPLOAD.name()));
		// find by card number
		if (!ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getCardNumber())) {
			predicate.and(qAcmMezaCard.cardNumber.eq(acmMezaCardDTO.getCardNumber()));
		}
		// find by id customer
		if (!ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getCustomerDTO())
				&& !ACMValidationUtils.isNullOrEmpty(acmMezaCardDTO.getCustomerDTO().getId())) {
			predicate.and(qAcmMezaCard.customer.id.eq(acmMezaCardDTO.getCustomerDTO().getId()));
		}
		// QueryDSL using springDATA
		Iterable<AcmMezaCard> iterable = acmMezaCardRepository.findAll(predicate);
		List<AcmMezaCard> acmMezaCards = new ArrayList<>();
		iterable.forEach(acmMezaCards::add);
		logger.info("{} : acmMezaCards was founded", acmMezaCards.size());

		// mapping returned list
		List<AcmMezaCardDTO> acmMezaCardDTOs = new ArrayList<>();
		acmMezaCards.forEach(
				acmMezaCard -> acmMezaCardDTOs.add(mapper.map(acmMezaCard, AcmMezaCardDTO.class)));
		logger.info("Returning founded data ...");
		return acmMezaCardDTOs;
	}

	/**
	 * Find customer having MEZA CARD.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@SuppressWarnings("unchecked")
	public List<Customer> findMezaCustomers() {

		// init list
		List<Customer> customers = new ArrayList<>();
		try {
			// execute native query

			// processing founded DATA
			// catch all customer with new and old WF
			Query query = entityManager.createNativeQuery(
					" SELECT distinct customer.ID_ACM_CUSTOMER FROM ACM_CUSTOMER customer "
							+ "INNER JOIN ACM_LOAN loan ON customer.ID_ACM_CUSTOMER = loan.ID_ACM_CUSTOMER"
							+ " INNER JOIN ACM_LOAN_INSTANCE loanIns ON loanIns.ID_ACM_LOAN = loan.ID_ACM_LOAN"
							+ " INNER JOIN ACM_WORKFLOW_STEP workFlow ON loanIns.ID_ACM_WORKFLOW_STEP = workFlow.ID_ACM_WORKFLOW_STEP"

							+ " WHERE workFlow.CHECK_MEZA_CARD = 1 "
							+ " AND customer.MEZA_CARD_STATUS "
							+ " = 'NEW' AND customer.ID_ACM_CUSTOMER in (select ACM_ID_CUSTOMER from ACM_MEZA_CARD)"
							+ "union "
							+ "SELECT customer.ID_ACM_CUSTOMER FROM ACM_CUSTOMER customer "
							+ "INNER JOIN ACM_LOAN loan ON customer.ID_ACM_CUSTOMER = "
							+ "loan.ID_ACM_CUSTOMER WHERE loan.ID_ACM_WORKFLOW_STEP IN (7,8,9,11,12,13,23) "
							+ "AND loan.STATUT_WORKFLOW NOT IN (14,15,17) AND customer.MEZA_CARD_STATUS IN "
							+ "('NEW') AND customer.ID_ACM_CUSTOMER in (select ACM_ID_CUSTOMER from ACM_MEZA_CARD)");

			List<BigInteger> queryResultList = query.getResultList();
			for (BigInteger bigInteger : queryResultList) {
				logger.info("ID_Customer = {}", bigInteger);
				customers.add(new Customer(bigInteger.longValue()));
			}
		}
		catch (Exception e) {
			logger.error("Failed to Get Customer list");
			e.printStackTrace();
		}
		logger.info("Founded Customers ={}", customers.size());
		return customers;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.MezaCardService#downloadReport(com.acm.utils.dtos.AcmMezaCardDTO)
	 */
	@Override
	public byte[] downloadReport(AcmMezaCardDTO acmMezaCardDTO) {

		// Find data by CustomerMezaCardStatus = NEW
		List<Customer> customerIds = findMezaCustomers();

		// find list from MEZA CARD table where status is ASSIGN for given customers
		List<AcmMezaCard> acmMezaCards = acmMezaCardRepository
				.findByCustomerInAndStatus(customerIds, MezaCardStatus.ASSIGN.name());

		// init instant datetime
		DateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy hh:mm a");
		String printedOn = dateFormat.format(new Date());
		final String SHEET = "Meza-card";
		final String[] headers = {"الرقم القومى", "الرقم المرجعى", "البلد", "الاسم بالعربى",
				"الاسم بالانجليزى", "الاسم بالانجليزى 2", "رقم التليفون", "العنوان بالعربى",
				"العنوان بالانجليزى", "العملة", "المبلغ الإجمالي"};

		try (Workbook workbook = new XSSFWorkbook();
				ByteArrayOutputStream out = new ByteArrayOutputStream();) {
			Sheet sheet = workbook.createSheet(SHEET);
			// Create a Font for styling header cells
			Font headerFont = workbook.createFont();
			headerFont.setBold(true);
			headerFont.setFontHeightInPoints((short) 12);
			headerFont.setColor(IndexedColors.WHITE.index);
			// Create a header CellStyle with the font
			CellStyle headerCellStyle = workbook.createCellStyle();
			headerCellStyle.setFont(headerFont);
			headerCellStyle.setFillBackgroundColor(IndexedColors.GREEN.index);
			headerCellStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
			headerCellStyle.setAlignment(HorizontalAlignment.CENTER);

			// Building EXCEL cell
			int rowIdx = 1;
			// DATE
			Row titleDateRow = sheet.createRow(rowIdx++);
			Cell cellTitleDateRow = titleDateRow.createCell(1);
			cellTitleDateRow.setCellValue("Printed On = " + printedOn);

			// TOTAL COUNT
			Row titleCountRow = sheet.createRow(rowIdx++);
			Cell cellTitleCountRow = titleCountRow.createCell(1);
			cellTitleCountRow.setCellValue("Total Count = " + acmMezaCards.size());

			// TOTAL AMOUNT : Fixed value 0 (To be use later in Payment process)
			Row titleAmountRow = sheet.createRow(rowIdx++);
			Cell cellTitleAmountRow = titleAmountRow.createCell(1);
			cellTitleAmountRow.setCellValue("Total Amount = 0");

			// Header
			rowIdx++;
			Row headerRow = sheet.createRow(rowIdx);
			for (int col = 0; col < headers.length; col++) {
				Cell cellHeaderRow = headerRow.createCell(col);
				cellHeaderRow.setCellValue(headers[col]);
				cellHeaderRow.setCellStyle(headerCellStyle);
			}

			// Body
			rowIdx++;
			for (AcmMezaCard acmMezaCard : acmMezaCards) {
				Row row = sheet.createRow(rowIdx++);
				// find customer data
				CustomerDTO customerDTO = creditClient.findById(acmMezaCard.getCustomer().getId());
				AddressDTO primaryAddress = new AddressDTO();
				if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getListAddress())) {
					primaryAddress = customerDTO.getListAddress().size() == 1
							? customerDTO.getListAddress().get(0)
							: customerDTO.getListAddress().stream()
									.filter(ad -> Boolean.TRUE.equals(ad.getIsPrimary()))
									.findFirst().orElse(new AddressDTO());
				}
				// getIdentity
				row.createCell(0).setCellValue(
						customerDTO.getIdentity() != null ? customerDTO.getIdentity() : "");
				// getCardNumber
				row.createCell(1).setCellValue(
						acmMezaCard.getCardNumber() != null ? acmMezaCard.getCardNumber() : "");
				// getCountry
				row.createCell(2).setCellValue("EGYPT");
				// getCorrespondanceName AR
				String[] nameSurName =
						StringUtils.convertingString(customerDTO.getCustomerName(), "\\|");
				StringBuffer correspondanceName = new StringBuffer();
				for (int i = 0; i < nameSurName.length; i++) {
					correspondanceName.append(' ' + nameSurName[i]);
				}
				row.createCell(3)
						.setCellValue(!ACMValidationUtils.isNullOrEmpty(correspondanceName)
								? correspondanceName.toString()
								: "");
				// getCorrespondanceName EN
				UserDefinedFieldsLinksDTO udfNameENPart1 = new UserDefinedFieldsLinksDTO();
				UserDefinedFieldsLinksDTO udfNameENPart2 = new UserDefinedFieldsLinksDTO();
				UserDefinedFieldsLinksDTO udfNameENPart3 = new UserDefinedFieldsLinksDTO();
				if (!ACMValidationUtils
						.isNullOrEmpty(customerDTO.getUserDefinedFieldsLinksDTOs())) {
					udfNameENPart1 = customerDTO.getUserDefinedFieldsLinksDTOs().stream()
							.filter(udf -> udf.getUserDefinedFieldsDTO().getName()
									.equals("Eng First Name Part One"))
							.findFirst().orElse(new UserDefinedFieldsLinksDTO());
					udfNameENPart2 = customerDTO.getUserDefinedFieldsLinksDTOs().stream()
							.filter(udf -> udf.getUserDefinedFieldsDTO().getName()
									.equals("Eng First Name Part Two"))
							.findFirst().orElse(new UserDefinedFieldsLinksDTO());
					udfNameENPart3 = customerDTO.getUserDefinedFieldsLinksDTOs().stream()
							.filter(udf -> udf.getUserDefinedFieldsDTO().getName()
									.equals("Eng First Name Part Three"))
							.findFirst().orElse(new UserDefinedFieldsLinksDTO());
				}
				String nameENPart1 =
						udfNameENPart1.getFieldValue() != null ? udfNameENPart1.getFieldValue()
								: "";
				String nameENPart2 =
						udfNameENPart2.getFieldValue() != null ? udfNameENPart2.getFieldValue()
								: "";
				String nameENPart3 =
						udfNameENPart3.getFieldValue() != null ? udfNameENPart3.getFieldValue()
								: "";
				char ENNamePart3 =
						!nameENPart3.isEmpty() ? nameENPart3.charAt(0) : new Character(' ');
				row.createCell(4).setCellValue(nameENPart1 + " " + nameENPart2 + " " + ENNamePart3);
				row.createCell(5).setCellValue(nameENPart1 + " " + nameENPart2 + " " + ENNamePart3);
				// getMobile
				row.createCell(6).setCellValue(
						customerDTO.getTelephone1() != null ? customerDTO.getTelephone1() : "");
				// getAddress AR primary
				row.createCell(7).setCellValue(primaryAddress.formattedAddress());
				// getAddress EN
				UserDefinedFieldsLinksDTO udfAddressEN =
						customerDTO.getUserDefinedFieldsLinksDTOs().stream()
								.filter(udf -> udf.getUserDefinedFieldsDTO().getName()
										.equals("Eng Customer Address"))
								.findFirst().orElse(new UserDefinedFieldsLinksDTO());
				row.createCell(8).setCellValue(
						udfAddressEN.getFieldValue() != null ? udfAddressEN.getFieldValue() : "");
				// Currency : Fixed Value "EGP"
				row.createCell(9).setCellValue("EGP");
				// Amount : Fixed Value 0 (To be user later in Payment Process)
				row.createCell(10).setCellValue("0");
			}

			// Setting Auto Column Width
			for (int i = 0; i < headers.length; i++) {
				sheet.autoSizeColumn(i);
				sheet.setColumnWidth(i, sheet.getColumnWidth(i) * 10 / 10);
			}

			// Generate file
			workbook.write(out);
			byte[] byteMezaCardReport = out.toByteArray();
			// Update row MezaCardStatus to SENT
			if (!ACMValidationUtils.isNullOrEmpty(byteMezaCardReport)
					&& byteMezaCardReport.length > 0) {
				logger.info("Updating ({}) Customers data : setting MezaCardStatus = SENT",
						customerIds.size());
				List<CustomerDTO> customersDTOs = new ArrayList<>();
				customerIds.forEach(
						customer -> customersDTOs.add(mapper.map(customer, CustomerDTO.class)));
				for (CustomerDTO customerDTO : customersDTOs) {
					customerDTO.setMezaCardStatus(CustomerMezaCardStatus.SENT.name());
					creditClient.updateMezaCardStatus(customerDTO);
				}
			}
			workbook.close();
			logger.info("Generating EXCEL REPORT for Meza-Card : {} :: DONE", SHEET);
			return byteMezaCardReport;
		}
		catch (IOException e) {
			throw new RuntimeException("fail to Export data to Excel file: " + e.getMessage());
		}
	}

}
