package com.acm.service.impl;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
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

import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.repository.MessageDetailsRepository;
import com.acm.service.SmsSenderService;
import com.acm.utils.dtos.MessageDetailsDTO;
import com.acm.utils.models.MessageDetails;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Preconditions;

/**
 * {@link SmsSenderServiceImpl} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Service
public class SmsSenderServiceImpl implements SmsSenderService {
	private static final Logger logger = LoggerFactory.getLogger(SmsSenderServiceImpl.class);

	/** The url orange tn send sms. */
	@Value("${rest.api.sms.service.send.url}")
	private String urlOrangeTnSendSms;

	/** The url orange tn login. */
	@Value("${rest.api.sms.service.login}")
	private String urlOrangeTnLogin;

	/** The url orange tn authorization key. */
	@Value("${rest.api.sms.service.authorizationKey}")
	private String urlOrangeTnAuthorizationKey;

	/** The sender phone number. */
	@Value("${rest.api.sms.service.sender.phone.number}")
	private String senderPhoneNumber;

	/** The rest template. */
	private RestTemplate restTemplate;

	@Autowired
	private MessageDetailsRepository messageDetailsRepository;
	@Autowired
	private DozerBeanMapper mapper;

	/**
	 * Instantiates a new sms sender service impl.
	 */
	public SmsSenderServiceImpl() {

		restTemplate = new RestTemplate();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_smsSender.service.SmsSenderService#loginAPIAbacus()
	 */
	@Override
	public String loginAPIAbacus() throws IOException {

		String apiUrl = urlOrangeTnLogin;

		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
		headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));
		headers.set("Authorization", urlOrangeTnAuthorizationKey);
		String data = "grant_type=client_credentials";
		byte[] postData = data.getBytes(StandardCharsets.UTF_8);
		int postDataLength = postData.length;
		headers.set("Content-Length", Integer.toString(postDataLength));

		HttpEntity<String> request = new HttpEntity<>(data, headers);
		ResponseEntity<String> response = restTemplate.postForEntity(apiUrl, request, String.class);
		ObjectMapper objectMapper = new ObjectMapper();
		JsonNode jsonNode = objectMapper.readTree(response.getBody());
		return jsonNode.get("access_token").asText();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_smsSender.service.SmsSenderService#sendSms(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public String sendSms(String customerPhone, String message) throws IOException {

		String apiUrl = urlOrangeTnSendSms;
		byte[] requestString = generateSmsSnderJSONObject(customerPhone, message);
		HttpHeaders headers = new HttpHeaders();
		headers.set("Authorization", "Bearer " + loginAPIAbacus());
		headers.setContentType(MediaType.APPLICATION_JSON);
		headers.set("Content-Type", "application/json");
		headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));
		HttpEntity<byte[]> request = new HttpEntity<>(requestString, headers);
		ResponseEntity<Object> responseAPI =
				restTemplate.exchange(apiUrl, HttpMethod.POST, request, Object.class);
		return responseAPI.toString();
	}

	/**
	 * Generate sms snder JSON object.
	 *
	 * @param customerPhone the customer phone
	 * @param message the message
	 * @return the JSON object
	 */
	private byte[] generateSmsSnderJSONObject(String customerPhone, String message) {

		JSONObject messageJson = new JSONObject();
		messageJson.put("message", message);
		JSONObject outboundSMSMessageRequestJsonObject = new JSONObject();
		outboundSMSMessageRequestJsonObject.put("address", "tel:" + customerPhone);
		outboundSMSMessageRequestJsonObject.put("senderAddress", "tel:" + senderPhoneNumber);
		outboundSMSMessageRequestJsonObject.put("senderName", "ACMnotif");
		outboundSMSMessageRequestJsonObject.put("outboundSMSTextMessage", messageJson);
		JSONObject jsonObject = new JSONObject();
		jsonObject.put("outboundSMSMessageRequest", outboundSMSMessageRequestJsonObject);

		String jsonString = jsonObject.toString();

		return jsonString.getBytes(Charset.forName("UTF-16"));

	}

	@Override
	public MessageDetailsDTO saveSms(MessageDetailsDTO messageDetailsDTO) {

		Preconditions.checkNotNull(messageDetailsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		MessageDetails messageDetails = mapper.map(messageDetailsDTO, MessageDetails.class);

		MessageDetails newMessageDetails = messageDetailsRepository.save(messageDetails);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, MessageDetails.class.getSimpleName());
		return mapper.map(newMessageDetails, MessageDetailsDTO.class);
	}

}
