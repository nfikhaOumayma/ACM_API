/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.File;
import java.io.FileInputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.persistence.EntityManager;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.web.multipart.MultipartFile;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.repository.AMLDataRepository;
import com.acm.service.AMLDataService;
import com.acm.utils.dtos.AMLDataDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.enums.ThirdPartyStatus;
import com.acm.utils.models.AMLData;
import com.acm.utils.models.QAMLData;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link AMLDataServiceImpl} Class Impl.
 *
 * @author HaythemBenizid
 * @since 1.0.0
 */
@Service
public class AMLDataServiceImpl implements AMLDataService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AMLDataServiceImpl.class);

	/** The aMLData repository. */
	@Autowired
	private AMLDataRepository aMLDataRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The entity manager. */
	@Autowired
	private EntityManager entityManager;

	/** The transaction manager. */
	@Autowired
	private PlatformTransactionManager transactionManager;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AMLDataService#find(com.acm.utils.dtos.AMLDataDTO)
	 */
	@Override
	public List<AMLDataDTO> find(AMLDataDTO aMLDataDTO) {

		Preconditions.checkNotNull(aMLDataDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// check search params are not Null Or Empty
		if (ACMValidationUtils.isNullOrEmpty(aMLDataDTO.getName())
				&& ACMValidationUtils.isNullOrEmpty(aMLDataDTO.getIdentityNumber())
				&& ACMValidationUtils.isNullOrEmpty(aMLDataDTO.getDateOfBirth())) {
			List<AMLDataDTO> aMLDatasDTOs = new ArrayList<>();
			aMLDatasDTOs.add(new AMLDataDTO(ThirdPartyStatus.ERROR.name(),
					ThirdPartyStatus.ERROR.name(), ThirdPartyStatus.ERROR.name()));
			return aMLDatasDTOs;
		}

		// init QAMLData
		QAMLData qAMLData = QAMLData.aMLData;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by Name
		if (!ACMValidationUtils.isNullOrEmpty(aMLDataDTO.getName())) {
			predicate.and(qAMLData.name.eq(aMLDataDTO.getName()));
		}

		// find like Identity Number
		if (!ACMValidationUtils.isNullOrEmpty(aMLDataDTO.getIdentityNumber())) {
			// Get Last Four Chars
			String lastFourChars = aMLDataDTO.getIdentityNumber()
					.substring(aMLDataDTO.getIdentityNumber().length() - 4);
			predicate.and(qAMLData.identityNumber.like("%" + lastFourChars + "%"));
		}

		// find by DateOfBirth => mast be in format : dd/MM/yyyy
		if (!ACMValidationUtils.isNullOrEmpty(aMLDataDTO.getDateOfBirth())) {
			predicate.and(qAMLData.dateOfBirth.eq(aMLDataDTO.getDateOfBirth()));
		}

		Iterable<AMLData> iterable = aMLDataRepository.findAll(predicate);
		List<AMLData> aMLDatas = new ArrayList<>();
		iterable.forEach(aMLDatas::add);
		logger.info("{} : AML-Data was founded", aMLDatas.size());
		if (ACMValidationUtils.isNullOrEmpty(aMLDatas)) {
			List<AMLDataDTO> aMLDatasDTOs = new ArrayList<>();
			aMLDatasDTOs.add(new AMLDataDTO(CommonConstants.NO_MATCH, CommonConstants.NO_MATCH,
					CommonConstants.NO_MATCH));
			return aMLDatasDTOs;
		}
		List<AMLDataDTO> aMLDatasDTOs = new ArrayList<>();
		aMLDatas.forEach(aMLData -> aMLDatasDTOs.add(mapper.map(aMLData, AMLDataDTO.class)));
		return aMLDatasDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AMLDataService#checkAML(com.acm.utils.dtos.AMLDataDTO)
	 */
	@Override
	public List<AMLDataDTO> checkAML(AMLDataDTO aMLDataDTO) {

		Preconditions.checkNotNull(aMLDataDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// check search params are not Null Or Empty
		if (ACMValidationUtils.isNullOrEmpty(aMLDataDTO.getName())) {
			List<AMLDataDTO> aMLDatasDTOs = new ArrayList<>();
			aMLDatasDTOs.add(new AMLDataDTO(ThirdPartyStatus.ERROR.name(),
					ThirdPartyStatus.ERROR.name(), ThirdPartyStatus.ERROR.name()));
			return aMLDatasDTOs;
		}

		// find by NAME
		List<AMLData> aMLDatas = aMLDataRepository.findByName(aMLDataDTO.getName());

		if (ACMValidationUtils.isNullOrEmpty(aMLDatas)) {
			logger.info("No Match founded by name : {}", aMLDataDTO.getName());
			List<AMLDataDTO> aMLDatasDTOs = new ArrayList<>();
			aMLDatasDTOs.add(new AMLDataDTO(CommonConstants.NO_MATCH, CommonConstants.NO_MATCH,
					CommonConstants.NO_MATCH));
			return aMLDatasDTOs;
		}
		logger.info("{} : AML-Data was founded", aMLDatas.size());
		List<AMLDataDTO> aMLDatasDTOs = new ArrayList<>();
		aMLDatas.forEach(aMLData -> aMLDatasDTOs.add(mapper.map(aMLData, AMLDataDTO.class)));
		return aMLDatasDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AMLDataService#save(com.acm.utils.dtos.AMLDataDTO)
	 */
	@Override
	public AMLDataDTO save(AMLDataDTO aMLDataDTO) {

		Preconditions.checkNotNull(aMLDataDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		AMLData aMLData = mapper.map(aMLDataDTO, AMLData.class);

		CommonFunctions.mapperToSave(aMLData, userClient, logger);
		AMLData newAMLData = aMLDataRepository.save(aMLData);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AMLData.class.getSimpleName());
		return mapper.map(newAMLData, AMLDataDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.AMLDataService#uploadAmlFile(org.springframework.web.multipart.MultipartFile[
	 * ])
	 */
	@Override
	public void uploadAmlFile(MultipartFile[] uploadedFiles) {

		Preconditions.checkNotNull(uploadedFiles, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// convert MultipartFile to File
		File file = null;
		if (uploadedFiles.length > 0) {
			for (MultipartFile multipartFile : uploadedFiles) {
				file = CommonFunctions.fileConverter(multipartFile,
						environment.getProperty("spring.servlet.multipart.location"));
			}
		}

		// call method to parse and process given FILE
		readParseSaveExcelFile(file);
	}

	/**
	 * Read && Parse && Processing founded data from excel file.
	 *
	 * @author HaythemBenizid
	 * @param excelFile the excel file
	 */
	private void readParseSaveExcelFile(File excelFile) {

		try {
			FileInputStream fileInputStream = new FileInputStream(excelFile);
			// find connected user
			UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
			// we create an XSSF Workbook object for our XLSX Excel File
			@SuppressWarnings("resource")
			XSSFWorkbook workbook = new XSSFWorkbook(fileInputStream);
			// we get first sheet
			XSSFSheet sheet = workbook != null ? workbook.getSheetAt(0) : null;
			// we iterate on rows
			Iterator<Row> rowIt = sheet != null ? sheet.iterator() : null;
			// init list
			List<AMLData> amlDatas = new ArrayList<>();
			int countRow = 1;
			String referenceCase = "";
			while (rowIt != null && rowIt.hasNext()) {
				Row row = rowIt.next();
				// iterate on cells for the current row
				Iterator<Cell> cellIterator = row.cellIterator(); // iterating over each column
				Boolean checkReference = Boolean.FALSE;
				Boolean checkName = Boolean.FALSE;
				AMLData amlData = new AMLData();
				while (cellIterator != null && cellIterator.hasNext()) {
					Cell cell = cellIterator.next();
					String cellValue = parseCellValue(cell);
					if (cell != null && !ACMValidationUtils.isNullOrEmpty(cellValue)) {
						// REFERENCE CASE (group by)
						if (cell.getColumnIndex() == 0 && cell.getCellType().equals(CellType.STRING)
								&& !cellValue.equals("م")) {
							referenceCase = cellValue;
						}
						// REFERENCE
						if (cell.getColumnIndex() == 0
								&& cell.getCellType().equals(CellType.NUMERIC)) {
							amlData.setReferenceInFile(Math.round(Double.parseDouble(cellValue)));
							checkReference = Boolean.TRUE;
						}
						// NAME
						if (cell.getColumnIndex() == 1) {
							amlData.setName(cellValue);
							checkName = Boolean.TRUE;
						}
						// IDENTITY
						if (cell.getColumnIndex() == 2) {
							amlData.setIdentityNumber(cellValue);
						}
						// DATE OF BIRTH
						if (cell.getColumnIndex() == 3) {
							amlData.setDateOfBirth(cellValue);
						}
						// UPDATE DATA
						if (cell.getColumnIndex() == 7) {
							amlData.setUpdatedData(cellValue);
						}
					}
				}
				// init list
				if (Boolean.TRUE.equals(checkName) && Boolean.TRUE.equals(checkReference)) {
					amlData.setReferenceCase(referenceCase);
					amlData.setInsertBy(userDTO.getFullName());
					amlData.setAcmVersion(0);
					amlData.setDateInsertion(new Date());
					amlData.setEnabled(Boolean.TRUE);
					amlDatas.add(amlData);
				}
				countRow++;
			}
			logger.info("countRow = {} / amlDatas size = {} ", countRow, amlDatas.size());

			// Processing founded data
			if (!ACMValidationUtils.isNullOrEmpty(amlDatas)) {
				// Delete existing AML data in ACM DB
				aMLDataRepository.deleteAll();

				// Reset Primary Key Value to "1" => Table ACM_AML_DATA
				executeQuery("DBCC CHECKIDENT (ACM_AML_DATA, RESEED, 0)");

				// savve all list
				aMLDataRepository.saveAll(amlDatas);
			}
		}
		catch (Exception e) {
			logger.error(e.getMessage());
			e.printStackTrace();
		}
	}

	/**
	 * Parses the cell value.
	 * 
	 * @author HaythemBenizid
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

	/**
	 * Execute given update query on TABLE.
	 *
	 * @author HaythemBenizid
	 * @param query the query
	 * @return the integer
	 */
	public Integer executeQuery(String query) {

		logger.info("EXECUTE QUERY = {} ", query);
		// executing given query
		TransactionTemplate transactionTemplate = new TransactionTemplate(transactionManager);
		return transactionTemplate.execute(transactionStatus -> {
			int status = entityManager.createNativeQuery(query).executeUpdate();
			transactionStatus.flush();
			return status;
		});
	}

}
