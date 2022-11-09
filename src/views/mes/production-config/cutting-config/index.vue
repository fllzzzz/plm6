<template>
  <div class="app-container">
    <el-row :gutter="10" id="laying-off-content">
      <el-col :xs="24" :sm="24" :md="24" :lg="9" :xl="9" style="margin-bottom: 10px">
        <laying-off-config @click-laying-off="handleChangeLayingOff" />
      </el-col>
      <el-col :xs="12" :sm="12" :md="12" :lg="6" :xl="6" style="margin-bottom: 10px">
        <el-card class="box-card team-card">
          <template v-slot:header class="clearfix card-header">
            <div style="display: flex; align-items: center; justify-content: space-between">
              <span style="display: flex; align-items: center">
                <span>切割配置列表</span>
              </span>
              <common-button
                size="mini"
                style="float: right; padding: 6px 10px; margin-bottom: 0px"
                type="primary"
                icon="el-icon-plus"
                @click="cutConfigRef?.toAdd"
              >
                新增
              </common-button>
            </div>
          </template>
          <cut-config
            ref="cutConfigRef"
            :layingOffRow="layingOffRow"
            @click-cut-config="handleChangeCutConfig"
          />
        </el-card>
      </el-col>
      <el-col :xs="12" :sm="12" :md="12" :lg="9" :xl="9" style="margin-bottom: 10px">
        <el-card class="box-card team-card">
          <template v-slot:header class="clearfix card-header">
            <div style="display: flex; align-items: center; justify-content: space-between">
              <span style="display: flex; align-items: center">
                <span>切割配置详情列表</span>
              </span>
              <common-button
                size="mini"
                style="float: right; padding: 6px 10px; margin-bottom: 0px"
                type="primary"
                icon="el-icon-plus"
                @click="cutConfigDetailRef?.toAdd"
              >
                新增
              </common-button>
            </div>
          </template>
          <cut-detail ref="cutConfigDetailRef" :cutConfigRow="cutConfigRow" @refresh-cut-config="cutConfigRef?.refresh"/>
        </el-card>
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import { provide, ref } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import layingOffConfig from './laying-off-config'
import cutConfig from './cut-config'
import cutDetail from './cut-detail'

const { maxHeight } = useMaxHeight({
  wrapperBox: ['.app-container', '#laying-off-content'],
  extraBox: ['.head-container', '.el-card__header'],
  paginate: true,
  extraHeight: 55
})

provide('maxHeight', maxHeight)

const cutConfigRef = ref()
const cutConfigDetailRef = ref()
const layingOffRow = ref({})
const cutConfigRow = ref({})

function handleChangeLayingOff(val) {
  if (val) {
    layingOffRow.value = val
    cutConfigRow.value = {}
  }
}

function handleChangeCutConfig(val) {
  console.log('handleChangeCutConfig', val)
  if (val) {
    cutConfigRow.value = val
  }
}
</script>
