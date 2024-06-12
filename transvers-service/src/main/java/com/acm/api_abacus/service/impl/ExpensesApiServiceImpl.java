/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service.impl;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;

import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.env.Environment;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.stereotype.Service;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestClientResponseException;
import org.springframework.web.client.RestTemplate;

import com.acm.api_abacus.service.ExpensesAPIService;
import com.acm.api_abacus.service.LoginApiService;
import com.acm.configuration.rest.RestTemplateConfig;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.ExpensesJournalPageDTO;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * {@link ExpensesApiServiceImpl} class.
 *
 * @author idridi
 * @since 1.0.8
 */
@Service
public class ExpensesApiServiceImpl implements ExpensesAPIService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(ExpensesApiServiceImpl.class);

	/** The environment. */
	@Autowired
	private Environment environment;

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The login api service. */
	@Autowired
	private LoginApiService loginApiService;

	/** The url serveur api. */
	@Value("${rest.api.abacus.url.server}")
	private String urlServeurApi;

	/** The uri create JournalPage. */
	@Value("${rest.api.abacus.journal.page.create.uri}")
	private String uriCreateJP;

	/** The uri post JournalPage. */
	@Value("${rest.api.abacus.journal.page.post.uri}")
	private String uriPostJP;

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.api_abacus.service.ExpensesAPIService#createExpenssesJournalPage(com.acm.utils.dtos.
	 * ExpensesJournalPageDTO)
	 */
	@Override
	public void createExpenssesJournalPage(ExpensesJournalPageDTO expensesJournalPageDTO)
			throws IOException, ApiAbacusException {

		// init resttemplate
		RestTemplate restTemplate;
		try {
			restTemplate = RestTemplateConfig.initRestTemplate();
		}
		catch (Exception e) {
			logger.error(" post Expenses Journal Page =  {}", e.getMessage());
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, e.getMessage());
		}
		// init headers
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.MULTIPART_FORM_DATA);
		headers.setAccept(Arrays.asList(MediaType.MULTIPART_FORM_DATA));

		// setting token
		headers = loginApiService.settingHttpHeaders(headers);
		// set data
		byte[] arrayOfByte = generateReport(expensesJournalPageDTO);
		MultiValueMap<String, Object> body = new LinkedMultiValueMap<>();
		body.add("journal", createTempFileResource(arrayOfByte));
		// init request
		HttpEntity<MultiValueMap<String, Object>> requestEntity = new HttpEntity<>(body, headers);
		// init URI
		String accessUrl = urlServeurApi + uriCreateJP;

		try {
			URI uri = new URI(accessUrl);
			logger.info("uri = {}", uri);
			// sending request to server using POST method
			ResponseEntity<String> response =
					restTemplate.postForEntity(uri, requestEntity, String.class);
			// mapping result
			logger.debug("{}", response.getBody());
		}
		catch (Exception e) {
			logger.error(" post Expenses Journal Page =  {}", e.getMessage());
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, e.getMessage());
		}

		// FIND ID JOURNAL PAGE BY ACCOUNT NUMBER
		Long idJP = findJournalPageId(expensesJournalPageDTO.getCreditAccount(),
				expensesJournalPageDTO.getDebitAccount());

		try {
			// post the expenses journal page
			postExpensesJournalPage(idJP, headers, restTemplate);
		}
		catch (Exception e) {
			logger.error(" post Expenses Journal Page =  {}", e.getMessage());
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, e.getMessage());
		}

	}

	/**
	 * Post expenses journal page.
	 * 
	 * @author idridi
	 * @param idExpensesJp the id expenses jp
	 * @param headers the headers
	 * @param restTemplate the rest template
	 * @throws URISyntaxException the URI syntax exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	private void postExpensesJournalPage(Long idExpensesJp, HttpHeaders headers,
			RestTemplate restTemplate) throws URISyntaxException, ApiAbacusException, IOException {

		try {
			// set header
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));
			// builldJson JournalPage
			JSONObject jsonObject = new JSONObject();
			jsonObject.put("journalPageID", idExpensesJp);
			jsonObject.put("isPostAndClose", "true");
			// init request
			HttpEntity<String> request = new HttpEntity<>(jsonObject.toString(), headers);
			// access uri
			String accessUrl = urlServeurApi + uriPostJP;
			URI uri = new URI(accessUrl);
			logger.info("uri = {}", uri);

			// sending request to server using POST method
			ResponseEntity<String> responseAPI =
					restTemplate.postForEntity(accessUrl, request, String.class);
			// mapping result
			logger.debug("{}", responseAPI.getBody());
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" post Expenses Journal Page RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" post Expenses Journal Page ResponseBodyAsString = {}",
					e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());
			final JsonNode jsonNode = new ObjectMapper().readTree(e.getResponseBodyAsString());
			String errorMsgAbacus =
					"{\"errorMessage\":\" " + CommonExceptionsMessage.ERROR_API_ABACUS + "\"}";
			if (jsonNode.isArray() && jsonNode.size() > 0) {
				JsonNode object = jsonNode.get(1);
				final JsonNode arrayJsonNode = object.get("validationMessages");
				if (arrayJsonNode.isNull()) {
					errorMsgAbacus = object.get("message").asText();
				}
				else {
					for (final JsonNode objNode : arrayJsonNode) {
						errorMsgAbacus = objNode.get("message").asText();
					}
				}
			}
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, errorMsgAbacus);
		}
	}

	/**
	 * Generate report.
	 * 
	 * @author idridi
	 * @param expensesJournalPageDTO the expenses journal page DTO
	 * @return the byte[]
	 */
	private byte[] generateReport(ExpensesJournalPageDTO expensesJournalPageDTO) {

		// The sheet name
		final String SHEET = "JournalEntries";
		final String[] columns = {"Description", "Reference", "ValueDate", "AccountNumber",
				"Credit", "Amount", "IBT"};
		// init instant datetime
		DateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
		String todayDate = dateFormat.format(new Date());

		try (XSSFWorkbook workbook = new XSSFWorkbook();
				ByteArrayOutputStream out = new ByteArrayOutputStream();) {
			Sheet sheet = workbook.createSheet(SHEET);
			int rowIdx = 0;
			// Create style
			CellStyle styleBold = workbook.createCellStyle();
			// Create font
			Font font = workbook.createFont();
			// Make font bold
			font.setBold(true);
			// set it to bold
			styleBold.setFont(font);
			// set border
			styleBold.setBorderBottom(BorderStyle.THIN);
			styleBold.setBorderLeft(BorderStyle.THIN);
			styleBold.setBorderRight(BorderStyle.THIN);
			styleBold.setBorderTop(BorderStyle.THIN);
			styleBold.setAlignment(HorizontalAlignment.CENTER);
			// Create style
			CellStyle stylerows = workbook.createCellStyle();

			// set border
			stylerows.setBorderBottom(BorderStyle.THIN);
			stylerows.setBorderLeft(BorderStyle.THIN);
			stylerows.setBorderRight(BorderStyle.THIN);
			stylerows.setBorderTop(BorderStyle.THIN);
			Row requestParamRow = sheet.createRow(rowIdx);
			for (int col = 0; col < columns.length; col++) {
				Cell cell = requestParamRow.createCell(col);
				cell.setCellValue(columns[col]);
				cell.setCellStyle(styleBold);

			}
			// first Row Debit Account
			rowIdx++;

			Row firstRow = sheet.createRow(rowIdx);

			// description
			Cell cell1Row1 = firstRow.createCell(0);
			cell1Row1.setCellStyle(stylerows);
			cell1Row1.setCellValue(!ACMValidationUtils
					.isNullOrEmpty(expensesJournalPageDTO.getExpensesDescription())
							? expensesJournalPageDTO.getExpensesDescription()
							: " ");

			// reference
			Cell cell2Row1 = firstRow.createCell(1);
			cell2Row1.setCellStyle(stylerows);
			cell2Row1.setCellValue(
					!ACMValidationUtils.isNullOrEmpty(expensesJournalPageDTO.getExpensesReference())
							? expensesJournalPageDTO.getExpensesReference()
							: " ");

			// date value
			Cell cell3Row1 = firstRow.createCell(2);
			cell3Row1.setCellStyle(stylerows);
			cell3Row1.setCellValue(todayDate);

			// account number DR
			Cell cell4Row1 = firstRow.createCell(3);
			cell4Row1.setCellStyle(stylerows);
			cell4Row1.setCellValue(expensesJournalPageDTO.getDebitAccount());

			// Credit : 0
			Cell cell5Row1 = firstRow.createCell(4);
			cell5Row1.setCellStyle(stylerows);
			cell5Row1.setCellValue(0);

			// Expenses amount
			Cell cell6Row1 = firstRow.createCell(5);
			cell6Row1.setCellStyle(stylerows);
			cell6Row1.setCellValue(
					!ACMValidationUtils.isNullOrEmpty(expensesJournalPageDTO.getAmount())
							? expensesJournalPageDTO.getAmount()
							: 0);

			// IBT : 0
			Cell cell7Row1 = firstRow.createCell(6);
			cell7Row1.setCellStyle(stylerows);
			cell7Row1.setCellValue(0);

			// Second Row Credit Account

			rowIdx++;
			Row secondRow = sheet.createRow(rowIdx);

			// description
			Cell cell1Row2 = secondRow.createCell(0);
			cell1Row2.setCellStyle(stylerows);
			cell1Row2.setCellValue(!ACMValidationUtils
					.isNullOrEmpty(expensesJournalPageDTO.getExpensesDescription())
							? expensesJournalPageDTO.getExpensesDescription()
							: " ");

			// reference
			Cell cell2Row2 = secondRow.createCell(1);
			cell2Row2.setCellStyle(stylerows);
			cell2Row2.setCellValue(
					!ACMValidationUtils.isNullOrEmpty(expensesJournalPageDTO.getExpensesReference())
							? expensesJournalPageDTO.getExpensesReference()
							: " ");

			// date value
			Cell cell3Row2 = secondRow.createCell(2);
			cell3Row2.setCellStyle(stylerows);
			cell3Row2.setCellValue(todayDate);

			// account number DR
			Cell cell4Row2 = secondRow.createCell(3);
			cell4Row2.setCellStyle(stylerows);
			cell4Row2.setCellValue(expensesJournalPageDTO.getCreditAccount());

			// Credit : 0
			Cell cell5Row2 = secondRow.createCell(4);
			cell5Row2.setCellStyle(stylerows);
			cell5Row2.setCellValue(1);

			// Expenses amount
			Cell cell6Row2 = secondRow.createCell(5);
			cell6Row2.setCellStyle(stylerows);
			cell6Row2.setCellValue(
					!ACMValidationUtils.isNullOrEmpty(expensesJournalPageDTO.getAmount())
							? expensesJournalPageDTO.getAmount()
							: 0);

			// IBT : 0
			Cell cell7Row2 = secondRow.createCell(6);
			cell7Row2.setCellStyle(stylerows);
			cell7Row2.setCellValue(0);

			// auto size cells
			for (int i = 0; i < columns.length + 3; i++) {
				sheet.autoSizeColumn(i);
			}

			// generate file
			workbook.write(out);
			return out.toByteArray();
			// create && save file in disk
			/*
			 * try (FileOutputStream outputStream = new
			 * FileOutputStream("C:\\test\\" + SHEET + ".xlsx")) { workbook.write(outputStream); }
			 * workbook.close();
			 */
		}
		catch (IOException e) {
			throw new RuntimeException(
					"fail to import data to Excel Schedule Report: " + e.getMessage());
		}
	}

	/**
	 * Creates the temp file resource.
	 * 
	 * @author idridi
	 * @param content the content
	 * @return the resource
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	static Resource createTempFileResource(byte[] content) throws IOException {

		Path tempFile = Files.createTempFile("journal", ".xlsx");
		Files.write(tempFile, content);
		return new FileSystemResource(tempFile.toFile());
	}

	/**
	 * Find journal page id.
	 * 
	 * @author idridi
	 * @param cr the cr credit account number
	 * @param dr the dr debit account number
	 * @return the long JournalPageID
	 */
	public Long findJournalPageId(String cr, String dr) {

		try {
			// load query
			String query = environment.getProperty("query.select.id.journal.page");
			logger.info(
					"findJournalPageId in given credit account number= {} and debit account number={}",
					cr, dr);

			// init params Credit Account cr And Debit Account dr
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("cr", cr).addValue("dr", dr);

			// execute query
			return namedParameterJdbcTemplate.queryForObject(query, namedParameters,
					new RowMapper<Long>() {
						@Override
						public Long mapRow(ResultSet rs, int rowNum) throws SQLException {

							return rs.getLong("JournalPageID");
						}
					});
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.GL.Account### {}", e.getMessage());
		}
		return 0L;
	}
}
