<template>
  <div class="app-container">
    <el-row :gutter="10" id="production-line-content">
      <el-col :xs="24" :sm="24" :md="24" :lg="12" :xl="12" style="margin-bottom: 10px">
        <line-config @click-line="handleChangeLine" />
      </el-col>
      <el-col :xs="12" :sm="12" :md="12" :lg="6" :xl="6">
        <el-card class="box-card team-card">
          <template v-slot:header class="clearfix card-header">
            <div style="display: flex; align-items: center; justify-content: space-between">
              <span style="display: flex; align-items: center">
                <span>生产组列表</span>
                <el-tooltip
                  v-if="currentLine.factoryName && currentLine.name"
                  class="item"
                  :content="`${currentLine.factoryName} - ${currentLine.name}`"
                  placement="top"
                >
                  <el-tag size="medium" style="margin-left: 10px; max-width: 180px; overflow: hidden; text-overflow: ellipsis">{{
                    `${currentLine.factoryName} - ${currentLine.name}`
                  }}</el-tag>
                </el-tooltip>
              </span>
              <common-button
                size="mini"
                style="float: right; padding: 6px 10px; margin-bottom: 0px"
                type="primary"
                icon="el-icon-edit"
                @click="groupRef?.toAdd"
              >
                新增
              </common-button>
            </div>
          </template>
          <group-config ref="groupRef" :line="currentLine" @click-group="handleChangeGroup" />
        </el-card>
      </el-col>
      <el-col :xs="12" :sm="12" :md="12" :lg="6" :xl="6">
        <el-card class="box-card team-card">
          <template v-slot:header class="clearfix card-header">
            <div style="display: flex; align-items: center; justify-content: space-between">
              <span style="display: flex; align-items: center">
                <common-radio-button
                  v-model="teamType"
                  :disabled="!currentGroup.name"
                  size="mini"
                  type="enum"
                  :unshowVal="currentLine.productType & componentTypeEnum.MACHINE_PART.V ? [teamTypeEnum.INSPECTION.V] : []"
                  :options="teamTypeEnum.ENUM"
                />
                <el-tooltip v-if="currentGroup.name" class="item" :content="`${currentGroup.name}`" placement="top">
                  <el-tag
                    v-if="currentGroup.name"
                    size="medium"
                    style="margin-left: 10px; max-width: 150px; overflow: hidden; text-overflow: ellipsis"
                    >{{ `${currentGroup.name}` }}</el-tag
                  >
                </el-tooltip>
              </span>
              <common-button
                v-if="teamType === teamTypeEnum.TEAM.V && teamRef && checkPermission(permission.edit) && currentGroup.id"
                size="mini"
                style="float: right; padding: 6px 10px; margin-bottom: 0px"
                type="primary"
                icon="el-icon-edit"
                @click="teamRef?.toAdd"
              >
                编辑
              </common-button>
              <common-button
                v-if="teamType === teamTypeEnum.INSPECTION.V && inspectionRef && checkPermission(permission.edit) && currentGroup.id"
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
            v-model="currentGroup.teamIds"
            v-if="teamType === teamTypeEnum.TEAM.V"
            ref="teamRef"
            :line="currentLine"
            :group="currentGroup"
            @change="productionTeamChange"
          />
          <inspection-config
            v-model="currentGroup.inspectionTeamIds"
            v-if="teamType === teamTypeEnum.INSPECTION.V"
            ref="inspectionRef"
            :line="currentLine"
            :group="currentGroup"
            @change="inspectionTeamChange"
          />
        </el-card>
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import { provide, ref } from 'vue'

import { teamTypeEnum, componentTypeEnum } from '@enum-ms/mes'
import checkPermission from '@/utils/system/check-permission'
import { configProductionLineGroupPM as permission } from '@/page-permission/config'

import useMaxHeight from '@compos/use-max-height'
import lineConfig from './line'
import groupConfig from './group'
import teamConfig from './team'
import inspectionConfig from './inspection'

const { maxHeight } = useMaxHeight({
  wrapperBox: ['.app-container', '#production-line-content'],
  extraBox: ['.head-container', '.el-card__header'],
  paginate: true,
  extraHeight: 55
})

provide('maxHeight', maxHeight)

const groupRef = ref()
const teamType = ref(teamTypeEnum.TEAM.V)
const teamRef = ref()
const inspectionRef = ref()
const currentLine = ref({})
const currentGroup = ref({})

function handleChangeLine(val) {
  if (val) {
    currentLine.value = val
    currentGroup.value = {}
    if (currentLine.value.productType & componentTypeEnum.MACHINE_PART.V) {
      teamType.value = teamTypeEnum.TEAM.V
    }
  }
}

function handleChangeGroup(val) {
  if (val) {
    currentGroup.value = val
  }
}

function productionTeamChange(val) {
  currentGroup.value.teamIds = val
}

function inspectionTeamChange(val) {
  currentGroup.value.inspectionTeamIds = val
}
</script>
