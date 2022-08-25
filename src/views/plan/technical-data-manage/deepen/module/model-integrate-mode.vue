<template>
  <div>
    <div class="head-container">
      <mHeader :queryMonomerId="info.id" :projectId="projectId">
        <template #viewLeft>
          <el-tag effect="plain" size="medium" class="filter-item">项目：{{ info.projectName }}</el-tag>
          <el-tag effect="plain" size="medium" class="filter-item">单体：{{ info.name }}</el-tag>
          <el-tag effect="plain" type="warning" size="medium" class="filter-item">
            模型版本号：{{ bimTeklaEditionEnum.VL[info.edition] }}
          </el-tag>
        </template>
      </mHeader>
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
      @selection-change="handleSelect"
    >
      <el-table-column type="selection" width="55" align="center" fixed :selectable="selectable" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="area.name" label="单元" align="center">
        <template #default="{ row }">
          <span>{{ row.area.name }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="translateStatus" label="模型状态" align="center">
        <template #default="{ row: { sourceRow: row } }">
          <el-tag v-if="row.translateStatus" effect="plain" style="margin-left: 5px" :type="translateStatusEnum.V[row.translateStatus].T">{{
            translateStatusEnum.VL[row.translateStatus]
          }}</el-tag>
          <span v-else>-</span>
        </template>
      </el-table-column>
      <el-table-column prop="integrationStatus" label="集成状态" align="center">
        <template #default="{ row: { sourceRow: row } }">
          <el-tag
            v-if="row.integrationStatus"
            effect="plain"
            style="margin-left: 5px"
            :type="integrationStatusEnum.V[row.integrationStatus].T"
            >{{ integrationStatusEnum.VL[row.integrationStatus] }}</el-tag
          >
          <span v-else>-</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column label="操作" width="170px" align="center" fixed="right">
        <template #default="{ row }">
          <div style="display: flex; justify-content: center">
            <upload-btn
              :upload-fun="upload"
              :data="{ monomerId: info?.id, areaId: row.area.id }"
              :fileClassify="undefined"
              :accept="'.ifc'"
              success-msg="模型导入成功"
              :btn-name="`${row?.hasModelImport ? '替换模型' : '导入模型'}`"
              btn-type="warning"
              btn-size="mini"
              style="margin-right: 5px"
              @success="handleSuccess"
            />
            <common-button
              v-permission="permission.model.del"
              v-if="row?.hasModelImport"
              type="danger"
              size="mini"
              style="margin-left: 5px"
              @click="delIt(row)"
            >
              删除
            </common-button>
          </div>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import { upload, modelDel, areaModelInfo } from '@/api/bim/model'
// import crudApi from '@/api/bim/model'
import { ref, defineProps, provide, defineExpose } from 'vue'
import { ElNotification, ElMessageBox } from 'element-plus'
import { deepenListPM as permission } from '@/page-permission/plan'

import {
  modelTranslateStatusEnum as translateStatusEnum,
  modelIntegrationStatusEnum as integrationStatusEnum,
  bimTeklaEditionEnum
} from '@enum-ms/bim'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import uploadBtn from '@comp/file-upload/SingleFileUploadBtn'
import mHeader from './components/model-integrate-mode-header'

const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  info: {
    type: Object,
    default: () => {}
  }
})

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, CRUD } = useCRUD(
  {
    title: '',
    sort: [],
    permission: { ...permission.model },
    optShow: { ...optShow },
    crudApi: { get: areaModelInfo, del: modelDel },
    queryOnPresenterCreated: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: false })

const areaIds = ref([])
const hasIntegrationModel = ref(false)
const hasProcessingIM = ref(false)
provide('areaIds', areaIds)
provide('info', props.info)
provide('hasProcessingIM', hasProcessingIM)
provide('hasIntegrationModel', hasIntegrationModel)

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.hasModelImport = Boolean(v.id)
    if (v.integrationStatus === integrationStatusEnum.SUCCESS.V || v.integrationStatus === integrationStatusEnum.PROCESSING.V) {
      hasIntegrationModel.value = true
    }
    if (v.integrationStatus === integrationStatusEnum.PROCESSING.V) {
      hasProcessingIM.value = true
    }
    return v
  })
}

CRUD.HOOK.beforeToQuery = () => {
  crud.query.monomerId = props.info.id
}

function selectable(row, rowIndex) {
  return (
    !(row.integrationStatus === integrationStatusEnum.SUCCESS.V || row.integrationStatus === integrationStatusEnum.PROCESSING.V) &&
    row.hasModelImport
  )
}

function handleSelect(selection, row) {
  areaIds.value = selection.map((v) => v.area.id)
}

function handleSuccess() {
  ElNotification({
    title: '上传成功',
    type: 'success',
    message: '模型导入成功',
    duration: 2500
  })
  crud.toQuery()
}

async function delIt(row) {
  ElMessageBox.confirm(`是否确认删除 “${row.area.name}” 单元下的模型`, '提示', {
    confirmButtonText: '确认',
    cancelButtonText: '取消',
    type: 'warning'
  }).then(async () => {
    try {
      await modelDel({ monomerId: props.info?.id, areaId: row.area.id })
      ElNotification({ title: '删除模型成功', type: 'success' })
      crud.toQuery()()
    } catch (error) {
      console.log('删除模型失败', error)
    }
  })
}

function fetchData() {
  crud.toQuery()
}

defineExpose({
  fetchData
})
</script>
