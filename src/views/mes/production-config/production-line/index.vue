<template>
  <div class="app-container">
    <el-row :gutter="10">
      <el-col :xs="24" :sm="24" :md="24" :lg="10" :xl="10" style="margin-bottom: 10px">
        <line-config @click-line="handleChangeline" />
      </el-col>
      <el-col :xs="24" :sm="24" :md="24" :lg="14" :xl="14">
        <el-card class="box-card team-card">
          <template v-slot:header class="clearfix card-header">
            <common-radio-button v-model="teamType" :disabled="!currentLine.name" size="mini" type="enum" :options="teamTypeEnum.ENUM" />
            <el-tag v-if="currentLine.factory && currentLine.name" size="medium">{{
              `${currentLine.factory.name} - ${currentLine.name}`
            }}</el-tag>
            <common-button
              v-if="teamType == teamTypeEnum.TEAM.V && teamRef && checkPermission(teamRef.permission.add) && currentLine.id"
              size="mini"
              style="float: right; padding: 6px 10px; margin-bottom: 0px"
              type="primary"
              icon="el-icon-plus"
              @click="teamRef.toAdd"
            >
              新增
            </common-button>
            <common-button
              v-if="
                teamType == teamTypeEnum.INSPECTION.V && inspectionRef && checkPermission(inspectionRef.permission.add) && currentLine.id
              "
              size="mini"
              style="float: right; padding: 6px 10px; margin-bottom: 0px"
              type="primary"
              icon="el-icon-plus"
              @click="inspectionRef.toAdd"
            >
              新增
            </common-button>
          </template>
          <team-config v-show="teamType == teamTypeEnum.TEAM.V" ref="teamRef" :line="currentLine" />
          <inspection-config v-show="teamType == teamTypeEnum.INSPECTION.V" ref="inspectionRef" :line="currentLine" />
        </el-card>
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import { reactive, ref } from 'vue'

import { teamTypeEnum } from '@enum-ms/mes'
import checkPermission from '@/utils/system/check-permission'

import lineConfig from './line'
import teamConfig from './team'
import inspectionConfig from './inspection'

const teamType = ref(teamTypeEnum.TEAM.V)
const teamRef = ref()
const inspectionRef = ref()
let currentLine = reactive({})

function handleChangeline(val) {
  if (val) {
    currentLine = Object.assign(currentLine, val)
  }
}
</script>
