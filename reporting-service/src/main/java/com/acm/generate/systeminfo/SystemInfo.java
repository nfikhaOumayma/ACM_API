/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.generate.systeminfo;

import java.io.IOException;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.net.UnknownHostException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Bean-Configuration to get System IP, name and MAC Address of windows system.
 *
 * @author HaythemBenizid
 * @since 1.0.10
 */
@Configuration
public class SystemInfo {

	/** Default Mode is INFO. */
	private static final Logger logger = LoggerFactory.getLogger(SystemInfo.class);

	/**
	 * Method for get System Name.
	 * 
	 * @author HaythemBenizid
	 * @return Host name
	 */
	@Bean
	public String getSystemName() {

		try {
			InetAddress inetaddress = InetAddress.getLocalHost(); // Get LocalHost refrence
			// Get Host Name
			return inetaddress.getHostName(); // return Host Name
		}
		catch (UnknownHostException E) {
			logger.error("Failed to get SystemName {}", E);
			return "";
		}
	}

	/**
	 * method to get Host IP.
	 * 
	 * @author HaythemBenizid
	 * @return Host IP Address
	 */
	@Bean
	public String getIPAddress() {

		try {
			InetAddress inetaddress = InetAddress.getLocalHost(); // Get LocalHost refrence
			// Get Host IP Address
			return inetaddress.getHostAddress(); // return IP Address
		}
		catch (UnknownHostException E) {
			logger.error("Failed to get IPAddress {}", E);
			return "";
		}
	}

	/**
	 * method to get Host Mac Address.
	 * 
	 * @author HaythemBenizid
	 * @return Mac Address
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@Bean
	public String getMAC() throws IOException {

		try {
			InetAddress ip = InetAddress.getLocalHost();
			NetworkInterface network = NetworkInterface.getByInetAddress(ip);
			byte[] mac = network.getHardwareAddress();
			StringBuilder s = new StringBuilder();
			if (mac != null) {
				for (int j = 0; j < mac.length; j++) {
					s.append(String.format("%02X%s", mac[j], (j < mac.length - (1)) ? "-" : ""));
				}
				// return MAc Address
				return s.toString();
			}
			else {
				logger.error("Address doesn't exist or is not accessible.");
				return "";
			}
		}
		catch (UnknownHostException | SocketException E) {
			logger.error("Failed to get MAC {}", E);
			return "";
		}
	}
}
