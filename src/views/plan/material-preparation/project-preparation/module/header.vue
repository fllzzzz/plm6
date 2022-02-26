<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <common-radio-button
        v-model="query.classification"
        :options="projectPreparationMatClsEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.preparationStatus"
        :options="preparationStatusEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <div class="filter-item">
        <el-tooltip :show-after="1000" content="有清单更新：未备料 或 清单更新时间“晚于”备料更新时间" placement="right">
          <el-checkbox v-model="query.hasListUpdate" label="只看有清单更新" size="small" border @change="crud.toQuery" />
        </el-tooltip>
      </div>
      <div class="filter-item">
          <el-checkbox v-model="query.boolPreparationLessThanList" label="只看备料量小于清单量" size="small" border @change="crud.toQuery" />
      </div>
      <br />
      <el-date-picker
        v-model="query.listUpdateTime"
        :default-time="defaultTime"
        type="daterange"
        range-separator=":"
        size="small"
        value-format="x"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        unlink-panels
        start-placeholder="清单更新日期"
        end-placeholder="清单更新日期"
        style="width: 240px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.preparationUpdateTime"
        :default-time="defaultTime"
        type="daterange"
        range-separator=":"
        size="small"
        value-format="x"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        unlink-panels
        start-placeholder="备料更新日期"
        end-placeholder="备料更新日期"
        style="width: 240px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-input
        v-model.trim="query.serialNumber"
        clearable
        style="width: 200px"
        size="small"
        placeholder="备料单号"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.listUploaderName"
        clearable
        style="width: 200px"
        size="small"
        placeholder="清单上传人"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.preparationUpdaterName"
        clearable
        style="width: 200px"
        size="small"
        placeholder="备料更新人"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <rr-operation />
    </div>
    <crud-operation>
      <template #viewLeft>
        <common-button v-permission="permission.config" size="mini" type="success" @click="configureProjectPreparationType">
          配置项目备料类型
        </common-button>
      </template>
    </crud-operation>
    <project-preparation-type-dlg v-model:visible="showPreparationTypeDlg" @update="crud.refresh" />
  </div>
</template>

<script setup>
import { inject, ref } from 'vue'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { preparationStatusEnum } from '@enum-ms/plan'
import { projectPreparationMatClsEnum } from '@/utils/enum/modules/classification'

import { regHeader } from '@compos/use-crud'
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import RrOperation from '@crud/RR.operation'
import CrudOperation from '@crud/CRUD.operation'

import projectPreparationTypeDlg from './configure-project-preparation-type/drawer.vue'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])
// 查询默认值
const defaultQuery = {
  listUpdateTime: [], // 清单更新时间
  preparationUpdateTime: [], // 备料更新时间
  hasListUpdate: false, // 只看有清单更新的列表（未备料 或 清单更新时间“晚于”备料更新时间）
  boolPreparationLessThanList: false, // 只看备料量小于清单量
  preparationStatus: undefined, // 备料状态
  classification: undefined, // 科目类型
  projectId: { value: undefined, resetAble: false }, // 项目id
  serialNumber: undefined, // 备料单号
  preparationUpdaterName: undefined, // 备料更新人
  listUploaderName: undefined // 清单上传人
}
// 权限
const permission = inject('permission')
// 显示配置dlg状态
const showPreparationTypeDlg = ref(false)
// 注册header组件
const { crud, query } = regHeader(defaultQuery)

// 项目监听
useGlobalProjectIdChangeToQuery(crud)

// 显示“配置项目备料方式”dlg
function configureProjectPreparationType() {
  showPreparationTypeDlg.value = true
}
</script>
