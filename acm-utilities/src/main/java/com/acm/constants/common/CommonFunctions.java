/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.constants.common;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.security.KeyFactory;
import java.security.PrivateKey;
import java.security.SecureRandom;
import java.security.spec.MGF1ParameterSpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Base64.Decoder;
import java.util.Base64.Encoder;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import javax.crypto.Cipher;
import javax.crypto.spec.OAEPParameterSpec;
import javax.crypto.spec.PSource;
import javax.persistence.Table;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.env.Environment;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.multipart.MultipartFile;

import com.acm.client.UserClient;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.AcmStatutsDTO;
import com.acm.utils.dtos.CustomMultipartFile;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.GenericModel;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/**
 * {@link CommonFunctions} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class CommonFunctions implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1800595712841477825L;

	/** Default Mode is INFO. */
	private static final Logger logger = LoggerFactory.getLogger(CommonFunctions.class);
	/** The in use meza cards. */
	private static Map<String, String> inUseMezaCards = new HashMap<>();

	/** The connected users session ids. Used in WebSocket Notification */
	private static Map<String, String> sessionIds = new HashMap<>();

	/**
	 * Convert {@link MultipartFile} to {@link File}.
	 * 
	 * @author HaythemBenizid
	 * @param file the file To convert
	 * @param basePath the base path
	 * @return the file converted
	 */
	public static File fileConverter(MultipartFile file, String basePath) {

		logger.debug("Begin convert MultipartFile to file ");
		DateTimeFormatter formatter = DateTimeFormatter
				.ofPattern("yyyy-MM-dd_HH-mm-ss_SSS" + (new Random().nextInt(9999 - 999) + 999));
		File convertedFile = new File(basePath + "/" + LocalDateTime.now().format(formatter) + "_"
				+ file.getOriginalFilename());
		FileOutputStream fileOutputStream = null;
		try {
			boolean createdFile = convertedFile.createNewFile();
			if (createdFile) {
				fileOutputStream = new FileOutputStream(convertedFile);
				fileOutputStream.write(file.getBytes());
				fileOutputStream.close();
			}
		}
		catch (IOException e) {
			logger.error("Exception has been occurred while converting file : {}", e.getMessage());
		}
		finally {
			try {
				if (fileOutputStream != null) {
					fileOutputStream.close();
				}
			}
			catch (IOException e) {
				logger.error("Exception has been occurred while closing stream file : {}",
						e.getMessage());
			}
		}
		logger.debug("Finish convert MultipartFile to file");
		return convertedFile;
	}

	/**
	 * Convert Byte[] to {@link File}.
	 *
	 * @author HaythemBenizid
	 * @param byteFile the byte file
	 * @param basePath the base path
	 * @param fileName the file name
	 * @return the file converted
	 */
	public static File fileConverter(byte[] byteFile, String basePath, String fileName) {

		logger.debug("Begin convert Byte[] to File");
		DateTimeFormatter formatter = DateTimeFormatter
				.ofPattern("yyyy-MM-dd_HH-mm-ss_SSS" + (new Random().nextInt(9999 - 999) + 999));
		File convertedFile =
				new File(basePath + "/" + LocalDateTime.now().format(formatter) + "_" + fileName);
		FileOutputStream fileOutputStream = null;
		try {
			boolean createdFile = convertedFile.createNewFile();
			if (createdFile) {
				fileOutputStream = new FileOutputStream(convertedFile);
				fileOutputStream.write(byteFile);
				fileOutputStream.close();
			}
		}
		catch (IOException e) {
			logger.error("Exception has been occurred while converting file : {}", e.getMessage());
		}
		finally {
			try {
				if (fileOutputStream != null) {
					fileOutputStream.close();
				}
			}
			catch (IOException e) {
				logger.error("Exception has been occurred while closing stream file : {}",
						e.getMessage());
			}
		}
		logger.debug("Finish convert Byte[] to File");
		return convertedFile;
	}

	/**
	 * Encode base 64.
	 * 
	 * @author HaythemBenizid
	 * @param stringToEncode the string to encode
	 * @return the string encoded
	 */
	public static String encodeBase64(String stringToEncode) {

		Encoder encoder = Base64.getEncoder();
		return encoder.encodeToString(stringToEncode.getBytes());
	}

	/**
	 * Decode base 64.
	 * 
	 * @author HaythemBenizid
	 * @param stringToDecode the string to decode
	 * @return the string decoded
	 */
	public static String decodeBase64(String stringToDecode) {

		Decoder decoder = Base64.getDecoder();
		byte[] decodedByte = decoder.decode(stringToDecode);
		return new String(decodedByte);
	}

	/**
	 * To singleton.
	 * 
	 * @author HaythemBenizid
	 * @param <T> the generic type
	 * @return the collector
	 */
	public static <T> Collector<T, ?, T> toSingleton() {

		return Collectors.collectingAndThen(Collectors.toList(), list -> {
			if (list.size() != 1) {
				throw new IllegalStateException();
			}
			return list.get(0);
		});
	}

	/**
	 * Return a list of elements distinct by specified property.
	 * 
	 * @author HaythemBenizid
	 * @param <T> the generic type
	 * @param keyExtractor the key extractor
	 * @return Predicate
	 */
	public static <T> Predicate<T> distinctByKey(Function<? super T, ?> keyExtractor) {

		Set<Object> seen = ConcurrentHashMap.newKeySet();
		return t -> seen.add(keyExtractor.apply(t));
	}

	/**
	 * Convert byte array to File Object.
	 *
	 * @author HaythemBenizid
	 * @param bytes the bytes
	 * @param basePath the base path
	 * @param environment the Environment
	 * @return the file
	 */
	public static File convertByteArrayToFile(byte[] bytes, String basePath,
			Environment environment) {

		// Log the begging of the process
		logger.debug("Begin convert: bytes -> File");
		// To avoid having two file with the same name, we append the current date with the file
		// name
		LocalDateTime now = LocalDateTime.now();
		DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd_HHmmss_SSS");
		// Create an empty File object linked to the default multipart file location
		File file = new File(environment.getProperty("spring.servlet.multipart.location") + "/"
				+ now.format(formatter) + "_" + basePath);
		try {
			// Fill the File object with the bytes array
			FileUtils.writeByteArrayToFile(file, bytes);
			// If everything is OK, return the File Object
			return file;
		}
		catch (IOException e) {
			// Log failure and stackTrace
			logger.error("cannot convert byte array to File Object {}", e.getMessage());
			// If An exception encountered, return null
			return null;
		}
	}

	/**
	 * Get the connected User.
	 *
	 * @author HaythemBenizid
	 * @param logger the logger
	 * @return the connected user
	 */
	public static UserDTO getConnectedUser(Logger logger) {

		try {
			// get authentication
			Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
			// init userDTO
			UserDTO userDTO = new UserDTO();
			try {
				// cast principal to UserDTO
				userDTO = (UserDTO) authentication.getPrincipal();
			}
			// if this method is called from microservice other than authentication-service then a
			// ClassCastException is thrown because : the type of pricipal in authentication
			// microservice is UserDTO ,while the type of principal in other microservices is the
			// userDTO.toString() : JSON format
			catch (ClassCastException e) {
				// parse authentication.getPrincipal() (JSON string) to UserDTO
				userDTO = userDTOSplitter(authentication.getPrincipal().toString(), logger);
			}

			logger.debug(CommonLoggerMessage.USER_CONNECTED, userDTO.getLogin());
			return userDTO;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_GETTING_CONNECTED_USER);
			return new UserDTO("ACM-APP-SYS", "ACM-BATCH", "SYSTEM", "admin@talys.com");
		}
	}

	/**
	 * User DTO splitter.
	 *
	 * @author mlamloum
	 * @param userDTOString the user DTO string
	 * @param logger the logger
	 * @return the user DTO
	 */
	private static UserDTO userDTOSplitter(String userDTOString, Logger logger) {

		// logger.info("BEGIN userDTO splitter method ()");
		// init gson
		Gson gson = new Gson();
		// init parser
		JsonParser parser = new JsonParser();
		// init userDTO
		UserDTO userDTO;
		// init simpleDateFormat
		SimpleDateFormat simpleDateFormat =
				new SimpleDateFormat("EEE MMM dd hh:mm:ss Z YYYY", Locale.US);
		// if the data is null then replace "null" with null in the json string
		userDTOString = userDTOString.replace("\"null\"", "null");
		// parse userDTOString to jsonObject
		JsonObject json = (JsonObject) parser.parse(userDTOString);
		// get hiringDate and resigningDate from the json string
		String hiringdate =
				json.get("hiringDate").isJsonNull() ? "" : json.get("hiringDate").getAsString();
		String resigningdate = json.get("resigningDate").isJsonNull() ? ""
				: json.get("resigningDate").getAsString();
		// remove hiringDate and resigningDate from json object because data of type util.Date could
		// not be parsed to UserDTO with gson
		json.remove("hiringDate");
		json.remove("resigningDate");
		// parse json object to UserDTO with gson
		userDTO = gson.fromJson(json.toString(), UserDTO.class);
		try {
			// if resigningDate is not empty, set it to userDTO
			if (!resigningdate.equals("null") && !resigningdate.equals("")) {
				Date resigningDate = simpleDateFormat.parse(resigningdate);
				userDTO.setResigningDate(resigningDate);
			}
			// if hiringDate is not empty, set it to userDTO
			if (!hiringdate.equals("null") && !hiringdate.equals("")) {
				Date hiringDate = simpleDateFormat.parse(hiringdate);
				userDTO.setHiringDate(hiringDate);
			}
		}
		catch (ParseException e) {
			e.printStackTrace();
		}
		// logger.info("END userDTO splitter method ()");
		return userDTO;
	}

	/**
	 * Is user not authenticated.
	 *
	 * @author HaythemBenizid
	 * @return the boolean
	 */
	public static Boolean isUserNotAuthenticated() {

		Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
		return authentication.getPrincipal().equals("anonymousUser")
				|| ACMValidationUtils.isNullOrEmpty(authentication.getCredentials());

	}

	/**
	 * Generate token.
	 *
	 * @author HaythemBenizid
	 * @param urlServeurAuthentification the url serveur authentification
	 * @return the string
	 */
	public static String generateToken(String urlServeurAuthentification) {

		logger.info("Generate Token for Back ACM ... ");
		ResponseEntity<String> response =
				generateToken(urlServeurAuthentification, "acmbatch", "acmbatch@123");
		try {
			// Get the Access Token from the received JSON response
			ObjectMapper mapper = new ObjectMapper();
			JsonNode node = mapper.readTree(response.getBody());
			String token = node.path("access_token").asText();
			logger.info("New Generated Token for Batch ACM : {}", token);
			return token;
		}
		catch (IOException e) {
			logger.error("Failed to generate token {}", e.getMessage());
		}
		return null;
	}

	/**
	 * Generate token for login.
	 *
	 * @param urlServeurAuthentification the url serveur authentification
	 * @param login the login
	 * @param password the password
	 * @return the string
	 */
	public static String generateTokenForLogin(String urlServeurAuthentification, String login,
			String password) {

		logger.info("Generate Token for Back ACM ... " + login);
		ResponseEntity<String> response =
				generateToken(urlServeurAuthentification, login, password);
		try {
			// Get the Access Token from the received JSON response
			ObjectMapper mapper = new ObjectMapper();
			JsonNode node = mapper.readTree(response.getBody());
			String token = node.path("access_token").asText();
			logger.info("New Generated Token for Batch ACM : {}", token);
			return token;
		}
		catch (IOException e) {
			logger.error("Failed to generate token {}", e.getMessage());
		}
		return null;
	}

	/**
	 * Generate token.
	 *
	 * @param urlServeurAuthentification the url serveur authentification
	 * @param login the login
	 * @param password the password
	 * @return the response entity
	 */
	public static ResponseEntity<String> generateToken(String urlServeurAuthentification,
			String login, String password) {

		RestTemplate restTemplate = new RestTemplate();
		String credentials = "talysclient:appclient@123";
		String encodedCredentials = new String(
				org.apache.commons.codec.binary.Base64.encodeBase64(credentials.getBytes()));
		HttpHeaders headers = new HttpHeaders();
		headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));
		headers.add("Authorization", "Basic " + encodedCredentials);
		HttpEntity<String> request = new HttpEntity<>(headers);
		String accessTokenUrl = urlServeurAuthentification
				+ "/oauth/token?grant_type=password&username=" + login + "&password=" + password;
		ResponseEntity<String> response =
				restTemplate.exchange(accessTokenUrl, HttpMethod.POST, request, String.class);
		if (response.getStatusCode() == HttpStatus.OK) {
			CommonLicenceVariable.currentSumiltaniousUser += 1;
		}
		return response;
	}

	/**
	 * Mapper to save. auto_init data (ENABLED=TRUE, DATE_INSERTION=current date,
	 * INSERT_BY=connected user, ACM_VERSION=0).
	 *
	 * @author HaythemBenizid
	 * @param model the model
	 * @param userClient the user client
	 * @param logger the logger
	 * @return the generic model
	 */
	public static GenericModel mapperToSave(GenericModel model, UserClient userClient,
			Logger logger) {

		UserDTO userDTO = getConnectedUser(logger);
		model.setInsertBy(userDTO.getFullName());
		model.setAcmVersion(0);
		model.setDateInsertion(new Date());
		model.setEnabled(Boolean.TRUE);
		return model;
	}

	/**
	 * Mapper to save.
	 *
	 * @param model the model
	 * @param userClient the user client
	 * @param logger the logger
	 * @param userDTO the user DTO
	 * @return the generic model
	 */
	public static GenericModel mapperToSave(GenericModel model, UserClient userClient,
			Logger logger, UserDTO userDTO) {

		if (ACMValidationUtils.isNullOrEmpty(userDTO)) {
			userDTO = getConnectedUser(logger);
		}
		model.setInsertBy(userDTO.getFullName());
		model.setAcmVersion(0);
		model.setDateInsertion(new Date());
		model.setEnabled(Boolean.TRUE);
		return model;
	}

	/**
	 * Mapper to update. auto_init data ( DATE_LAST_UPDATE=current date, UPDATED_BY=connected user,
	 * ACM_VERSION=+1).
	 *
	 * @author HaythemBenizid
	 * @param model the model
	 * @param userClient the user client
	 * @param logger the logger
	 * @return the generic model
	 */
	public static GenericModel mapperToUpdate(GenericModel model, UserClient userClient,
			Logger logger) {

		UserDTO userDTO = getConnectedUser(logger);
		model.setUpdatedBy(userDTO.getFullName());
		model.setDateLastUpdate(new Date());
		model.setAcmVersion(model.getAcmVersion() != null ? model.getAcmVersion() + 1 : 0);
		return model;
	}

	/**
	 * Mapper to update. auto_init data ( DATE_LAST_UPDATE=current date, UPDATED_BY=connected user,
	 * ACM_VERSION=+1, enabled from model).
	 *
	 * @author MoezMhiri
	 * @param model the model
	 * @param userClient the user client
	 * @param logger the logger
	 * @return the generic model
	 */
	public static GenericModel mapperToUpdateWithEnabled(GenericModel model, UserClient userClient,
			Logger logger) {

		UserDTO userDTO = getConnectedUser(logger);
		model.setUpdatedBy(userDTO.getFullName());
		model.setDateLastUpdate(new Date());
		model.setAcmVersion(model.getAcmVersion() != null ? model.getAcmVersion() + 1 : 0);
		model.setEnabled(model.getEnabled());
		return model;
	}

	/**
	 * Mapper to update with enabled.
	 *
	 * @param model the model
	 * @param userClient the user client
	 * @param logger the logger
	 * @param userDTO the user DTO
	 * @return the generic model
	 */
	public static GenericModel mapperToUpdateWithEnabled(GenericModel model, UserClient userClient,
			Logger logger, UserDTO userDTO) {

		if (ACMValidationUtils.isNullOrEmpty(userDTO)) {
			userDTO = getConnectedUser(logger);
		}
		model.setUpdatedBy(userDTO.getFullName());
		model.setDateLastUpdate(new Date());
		model.setAcmVersion(model.getAcmVersion() != null ? model.getAcmVersion() + 1 : 0);
		model.setEnabled(model.getEnabled());
		return model;
	}

	/**
	 * Mapping status : convert from comma separate String (key , value) to {@link AcmStatutsDTO}
	 * object.
	 * 
	 * @author HaythemBenizid
	 * @param commonStatus the common status
	 * @return the acm statuts DTO
	 */
	public static AcmStatutsDTO mappingStatus(String commonStatus) {

		// step 1 : converting comma separate String to array of String
		String[] elements = commonStatus.split(",");
		// step 2 : returning mapping object
		return new AcmStatutsDTO(Integer.parseInt(elements[0].trim()), elements[1].trim());
	}

	/**
	 * Load status : returning status from given list by key.
	 * 
	 * @author HaythemBenizid
	 * @param key the key
	 * @param acmStatutsDTOs the acm statuts DT os
	 * @return the acm statuts DTO
	 */
	public static AcmStatutsDTO loadStatus(Integer key, List<AcmStatutsDTO> acmStatutsDTOs) {

		return acmStatutsDTOs.stream().filter(status -> key.equals(status.getKey())).findAny()
				.orElse(null);
	}

	/**
	 * Gets the table name from class.
	 *
	 * @author HaythemBenizid
	 * @param clazz the clazz
	 * @return the table name from class
	 */
	public static String getTableNameFromClass(Class<?> clazz) {

		return clazz.getAnnotation(Table.class).name();
	}

	/**
	 * Convert object to JSON string.
	 * 
	 * @author HaythemBenizid
	 * @param object the object
	 * @return the string
	 */
	public static String convertObjectToJSONString(Object object) {

		try {
			ObjectMapper mapper = new ObjectMapper();
			return mapper.writeValueAsString(object);
		}
		catch (JsonProcessingException e) {
			logger.error("### JsonProcessingException : {}", e.getMessage());
		}
		return null;
	}

	/**
	 * Convert JSON string to object.
	 * 
	 * @author HaythemBenizid
	 * @param jSONString the j SON string
	 * @param clazz the clazz
	 * @return the object
	 */
	public static Object convertJSONStringtoObject(String jSONString, Class<?> clazz) {

		// Creating a Gson Object with Date config
		JsonDeserializer<Date> dateJsonDeserializer =
				(json, typeOfT, context) -> json == null ? null : new Date(json.getAsLong());
		Gson gson =
				new GsonBuilder().registerTypeAdapter(Date.class, dateJsonDeserializer).create();
		// Converting json to object && returning object
		return gson.fromJson(jSONString, clazz);
	}

	/**
	 * Generate random STRING by given length (LENGTH MUST BE >= 4).String = CHAR_LOWER + CHAR_UPPER
	 * + NUMBER.
	 * 
	 * @author HaythemBenizid
	 * @param length the length
	 * @return the string
	 */
	public static String generateRandomString(Integer length) {

		SecureRandom random = new SecureRandom();
		final String CHAR_LOWER = "abcdefghijklmnopqrstuvwxyz";
		final String CHAR_UPPER = CHAR_LOWER.toUpperCase();
		final String NUMBER = "0123456789";
		final String DATA_FOR_RANDOM_STRING = CHAR_LOWER + CHAR_UPPER + NUMBER;
		// check length
		if (length < 4) {
			throw new IllegalArgumentException();
		}
		// building random login
		StringBuilder sb = new StringBuilder(length);
		for (int i = 0; i < length; i++) {
			// random returns 0-61
			int rndCharAt = random.nextInt(DATA_FOR_RANDOM_STRING.length());
			char rndChar = DATA_FOR_RANDOM_STRING.charAt(rndCharAt);
			sb.append(rndChar);
		}
		logger.debug("Generated Login : {}", sb);
		return sb.toString();
	}

	/**
	 * Calculate and format pourcentage.
	 * 
	 * @author HaythemBenizid
	 * @param number1 the number 1
	 * @param number2 the number 2
	 * @param places the places
	 * @return the float
	 */
	public static Float calculateAndFormatPourcentage(Float number1, Float number2,
			Integer places) {

		if (number2 == 0) {
			return 0F;
		}
		float pourcentage = (number1 / number2) * 100;
		double scale = Math.pow(10, places);
		Double pourcentageScaled = Math.round(pourcentage * scale) / scale;
		return pourcentageScaled.floatValue();
	}

	/**
	 * Find matched field (IngoreCase) in class.
	 *
	 * @author HaythemBenizid
	 * @param clazz the clazz
	 * @param field the field
	 * @return the field
	 */
	public static Field findMatchedFieldIngoreCase(Class<?> clazz, String field) {

		Field[] fields = clazz.getDeclaredFields();
		for (Field f : fields) {
			if (f.getName().equalsIgnoreCase(field)) {
				return f;
			}
		}
		return null;
	}

	/**
	 * convert Object To JSON (USED in JUNITTest).
	 *
	 * @author HaythemBenizid
	 * @param object the object
	 * @return the byte[]
	 * @throws Exception the exception
	 */
	public static byte[] toJson(Object object) throws Exception {

		ObjectMapper objectMapper = new ObjectMapper();
		return objectMapper.writeValueAsBytes(object);
	}

	/**
	 * Gets the in use meza cards.
	 *
	 * @author ManelLamloum
	 * @return the in use meza cards
	 */
	public static Map<String, String> getInUseMezaCards() {

		return inUseMezaCards;
	}

	/**
	 * In use meza cards contains.
	 *
	 * @param mezaCardNumber the meza card number
	 * @return the boolean
	 */
	public static Boolean inUseMezaCardsContains(String mezaCardNumber) {

		return inUseMezaCards.containsValue(mezaCardNumber);
	}

	/**
	 * Sets the in use meza cards.
	 * 
	 * @author ManelLamloum
	 * @param inUseMezaCards the new in use meza cards
	 */
	public static void setInUseMezaCards(Map<String, String> inUseMezaCards) {

		CommonFunctions.inUseMezaCards = inUseMezaCards;
	}

	/**
	 * Adds the in use meza card.
	 *
	 * @author ManelLamloum
	 * @param login the login
	 * @param mezaCardNumber the meza card number
	 * @return the list
	 */
	public static Map<String, String> addInUseMezaCard(String login, String mezaCardNumber) {

		CommonFunctions.inUseMezaCards.put(login, mezaCardNumber);
		return inUseMezaCards;
	}

	/**
	 * Delete from in use meza cards.
	 *
	 * @author ManelLamloum
	 * @param login the login
	 */
	public static void deleteFromInUseMezaCards(String login) {

		CommonFunctions.inUseMezaCards.remove(login);
	}

	/**
	 * Gets the session ids.
	 *
	 * @return the session ids
	 */
	public static Map<String, String> getSessionIds() {

		return sessionIds;
	}

	/**
	 * Gets the session id.
	 *
	 * @param username the username
	 * @return the session id
	 */
	public static String getSessionId(String username) {

		return sessionIds.get(username);
	}

	/**
	 * Adds the session id.
	 *
	 * @param username the username
	 * @param sessionId the session id
	 * @return the map
	 */
	public static Map<String, String> addSessionId(String username, String sessionId) {

		CommonFunctions.sessionIds.put(username, sessionId);
		return sessionIds;
	}

	/**
	 * Delete from session ids.
	 *
	 * @param username the username
	 */
	public static void deleteFromSessionIds(String username) {

		CommonFunctions.sessionIds.remove(username);
	}

	/**
	 * Decrypt with private key.
	 *
	 * @param input the input
	 * @return the string
	 * @throws Exception the exception
	 */
	public static String decryptWithPrivateKey(String input) throws Exception {

		byte[] privateKeyBytes = Base64.getDecoder().decode(
				"MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQDEfK+ydR02HMFxlcCzh6LNTQm+ZCqIo0jqt2ioWOpe1wHtWzMWF9/dinH4GvmD8mCN7RC8MRiQDEn2BIMGLT+KHIYUtrgmTYHh+r59TT0r6aYDaahP4uZrYBhXUT0RsNCQ0S5oUCwNxc1/3DPWzdn1ZnFQqIxHt89tafI9aa/RxUlrmTEjYqlrM5xXhScwwrUCN4RfvmO/QFLCFzXrKt5X9fU8w6YolC2XBULuYEDz2NEoWMyJLIHLzt/i4HnlAND7CQcBqKQ3RENiwhZb6r697IE6exqcKgNxhUlffGyKyhSNXnD5RXXPRlPl5+8XSA6WK7/FK6lc/+jCgkZD8naFAgMBAAECggEBAKheJ7gF/vgFgAWyyyOL0IeXwgexkXjwYmUusLHBL2QBXZdklMwTvTL7lVmC09iq8u3q/HImp+OXhDEaAKRkVZQliQdq4zV/KRLZaC265/YfCrDV8vIxfjz4ZSM9a9Ui7DWL6fZc7MMIgmVT7d/dGS3dkLALHdm+ujIJnRfe3lgi8pVaYcPqhKw1PzfbElR/+w2q+uFK5KXBRvjnzE5CnGOhrLnGyCi2PaQlYD3iexOXSp5woDip6s+07e0LbqkJo0JiSGP1E3aVtUXWH8256wCzODQS7RsjBg2YMy5Ds9PfmNPn6ZR9JziQohGiIbuY1gY+0NoEuurDEzoNHZhApkECgYEA8mY2F9ubwMTFFQZ7PVnHWzAQ6O3KN2dmxPGVMjdEH3SJB2wPnxWMAnOdw7HlN2b/cIiC99brtPslABp6kg/2lnl3ZfN8xjZSjMUKkFHpG3hH3FyKjyiCMSKbafPYCMt2jRMorMjB/p/Fp8eP+wyq4mtk8MHyCD0zsLq0owlLXx0CgYEAz4L/ooTsn6oGNoP3OPN7B3xUwXGwwJW1vRFYbPpfY8VIsiMR1Yi9zGaAcud1xbfoXXPr5oFbeFZiwMjgBZafaS04H2ibTKjuYIsVjzGAyocREek9XpOzocQMogFeakuflMW9+otFcWprD9TXHziweahl0x7hGQakwo0TcvmZ0IkCgYEAx4JzIV3iq14JbI3CH7PmTtqYAG+/kaxsvMquZv9v9YfgRRifHuwIKfhpMKoaJGiE9GWXjk/5/yL6yEQDAHddGWruaB5pY2dQuIZxdGnGuJJeazRL4xqPVcvLKQDHni2A7QqI3vX6Hg35DGhHzHUXrXaBRVwMTtW41sxFYy+CNs0CgYAo2htFfXGwBSLv/I7hEV35HUr6Qk2PvGlo7U9BNREubRpgp3LNXPwwj0VeWZ7f69K9AyM/39uIF0mdtm4T65E6fYJ8/HUGZIN5UihOZmp6dJixs/827VEB2by68BYMSxMBkORTvLNRHqgxF6+6H8juSm6xX3TlHl+1RVy819KmMQKBgGPSWjchFeFbDB6RFk7WoDzNkgqfasTYP9WJiPuaJwWfODPGp+fpd6ABntmHRGNfIcSAQ/Rg8SxewBD1Oebb9D1CEg4/y2EZQcZXvS5Uu0lHGbvnS+ynyCwF+0JCJJQMKFAJuLKL7Fr5NhK0UsOCTPqZsgmcok1cmlXu7Ksf/7P/");

		PKCS8EncodedKeySpec keySpecP = new PKCS8EncodedKeySpec(privateKeyBytes);
		KeyFactory keyFactory = KeyFactory.getInstance("RSA");
		PrivateKey privateKey = keyFactory.generatePrivate(keySpecP);

		byte[] decodedBytesModule = Base64.getDecoder().decode(input.split(" ")[0]);
		byte[] decodedBytesParam = Base64.getDecoder().decode(input.split(" ")[1]);

		Cipher cipher = Cipher.getInstance(CommonConstants.TRANSFORMATION);
		OAEPParameterSpec oaepParameterSpec = new OAEPParameterSpec("SHA-256", "MGF1",
				MGF1ParameterSpec.SHA256, PSource.PSpecified.DEFAULT);
		cipher.init(Cipher.DECRYPT_MODE, privateKey, oaepParameterSpec);
		byte[] decryptedBytesModule = cipher.doFinal(decodedBytesModule);
		byte[] decryptedBytesParam = cipher.doFinal(decodedBytesParam);

		return new String(decryptedBytesModule, StandardCharsets.UTF_8) + ","
				+ new String(decryptedBytesParam, StandardCharsets.UTF_8);
	}

	/**
	 * Convert to multipart files.
	 *
	 * @param byteArray the byte array
	 * @return the multipart file[]
	 */
	public static MultipartFile[] convertToMultipartFiles(byte[] byteArray) {

		MultipartFile customMultipartFile = new CustomMultipartFile(byteArray);
		List<MultipartFile> multipartFiles = new ArrayList<>();
		multipartFiles.add(customMultipartFile);
		return multipartFiles.toArray(new MultipartFile[0]);
	}

	/**
	 * find Configuration From List.
	 * 
	 * @author HaythemBenizid
	 * @param environnementDTOs the environnement DT os
	 * @param key the key
	 * @param defaultValue the default value
	 * @return the string
	 */
	public static String findConfigurationFromList(List<AcmEnvironnementDTO> environnementDTOs,
			String key, String defaultValue) {

		try {
			defaultValue =
					environnementDTOs.stream().filter(predicate -> predicate.getKey().equals(key))
							.findFirst().get().getValue();
		}
		catch (Exception e) {
			logger.error("NO Configuration was founded for KEY ={}", key);
			e.printStackTrace();
		}
		logger.debug("findConfigurationFromList : {} : {}", key, defaultValue);
		return defaultValue;
	}
}
