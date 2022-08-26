<template>
  <div class="app-container">
    <el-row :gutter="10" id="production-line-content">
      <el-col :xs="24" :sm="24" :md="24" :lg="12" :xl="12" style="margin-bottom: 10px">
        <line-config @click-line="handleChangeLine" />
      </el-col>
      <el-col :xs="24" :sm="24" :md="24" :lg="12" :xl="12">
        <el-card class="box-card team-card">
          <template v-slot:header class="clearfix card-header">
            <div style="display: flex; align-items: center; justify-content: space-between">
              <span style="display: flex; align-items: center">
                <common-radio-button
                  v-model="teamType"
                  :disabled="!currentLine.name"
                  size="mini"
                  type="enum"
                  :options="teamTypeEnum.ENUM"
                />
                <el-tag v-if="currentLine.factoryName && currentLine.name" size="medium" style="margin-left: 10px">{{
                  `${currentLine.factoryName} - ${currentLine.name}`
                }}</el-tag>
              </span>

              <common-button
                v-if="teamType === teamTypeEnum.TEAM.V && teamRef && checkPermission(permission.edit) && currentLine.id"
                size="mini"
                style="float: right; padding: 6px 10px; margin-bottom: 0px"
                type="primary"
                icon="el-icon-edit"
                @click="teamRef?.toAdd"
              >
                编辑
              </common-button>
              <common-button
                v-if="teamType === teamTypeEnum.INSPECTION.V && inspectionRef && checkPermission(permission.edit) && currentLine.id"
                size="mini"
                style="float: right; padding: 6px 10px; margin-bottom: 0px"
                type="primary"
                icon="el-icon-edit"
                @click="inspectionRef?.toAdd"
              >
                编辑
              </common-button>
            </div>
          </template>
          <team-config
            v-model="currentLine.teamIds"
            v-if="teamType === teamTypeEnum.TEAM.V"
            ref="teamRef"
            :line="currentLine"
            @change="productionTeamChange"
          />
          <inspection-config
            v-model="currentLine.inspectionTeamIds"
            v-if="teamType === teamTypeEnum.INSPECTION.V"
            ref="inspectionRef"
            :line="currentLine"
            @change="inspectionTeamChange"
          />
        </el-card>
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import { provide, ref } from 'vue'

import { teamTypeEnum } from '@enum-ms/mes'
import checkPermission from '@/utils/system/check-permission'
import { configProductionLinePM as permission } from '@/page-permission/config'

import useMaxHeight from '@compos/use-max-height'
import lineConfig from './line'
import teamConfig from './team'
import inspectionConfig from './inspection'

const { maxHeight } = useMaxHeight({
  wrapperBox: ['.app-container', '#production-line-content'],
  extraBox: ['.head-container', '.el-card__header'],
  paginate: true,
  extraHeight: 55
})

provide('maxHeight', maxHeight)

const teamType = ref(teamTypeEnum.TEAM.V)
const teamRef = ref()
const inspectionRef = ref()
const currentLine = ref({})

function handleChangeLine(val) {
  if (val) {
    currentLine.value = val
  }
}

function productionTeamChange(val) {
  currentLine.value.teamIds = val
}

function inspectionTeamChange(val) {
  currentLine.value.inspectionTeamIds = val
}
</script>
