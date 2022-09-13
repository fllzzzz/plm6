<template>
  <common-table v-loading="tableLoading" :data="tableData" style="width: 100%">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column prop="projectName" :show-overflow-tooltip="true" label="项目" align="center">
      <template #default="{ row: { sourceRow: row } }">
        <span>{{ row.projectName }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="monomerName" :show-overflow-tooltip="true" label="单体" align="center">
      <template #default="{ row: { sourceRow: row } }">
        <span>{{ row.monomerName }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="edition" :show-overflow-tooltip="true" label="版本号" align="center">
      <template #default="{ row: { sourceRow: row } }">
        <span v-if="row?.edition">{{ bimTeklaEditionEnum.VL[row?.edition] }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="translateStatus" :show-overflow-tooltip="true" label="模型状态" align="center">
      <template #default="{ row: { sourceRow: row } }">
        <el-tag v-if="row?.translateStatus" effect="plain" :type="translateStatusEnum.V[row.translateStatus].T">{{
          translateStatusEnum.VL[row.translateStatus]
        }}</el-tag>
        <span v-else>-</span>
      </template>
    </el-table-column>
    <el-table-column label="操作" align="center" width="170">
      <template #default="{ row }">
        <div style="display: flex; justify-content: center">
          <upload-btn
            :upload-fun="upload"
            :data="{ monomerId: info?.id }"
            :fileClassify="undefined"
            :accept="'.ifc'"
            success-msg="模型导入成功"
            :btn-name="`${row?.hasModelImport ? '替换模型' : '导入模型'}`"
            btn-type="warning"
            btn-size="mini"
            @success="handleSuccess"
          />
          <common-button v-if="row?.hasModelImport" type="danger" size="mini" style="margin-left: 5px" @click="delIt(row)">
            删除
          </common-button>
        </div>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { upload, monomerModelInfo, modelDel } from '@/api/bim/model'
import { ElNotification, ElMessageBox } from 'element-plus'
import { defineProps, defineExpose, ref } from 'vue'

import { modelTranslateStatusEnum as translateStatusEnum, bimTeklaEditionEnum } from '@enum-ms/bim'
import uploadBtn from '@comp/file-upload/SingleFileUploadBtn'
import { isNotBlank } from '@/utils/data-type'

const props = defineProps({
  info: {
    type: Object,
    default: () => {}
  }
})

const tableLoading = ref(false)
const tableData = ref([])

function handleSuccess() {
  ElNotification({
    title: '上传成功',
    type: 'success',
    message: '模型导入成功',
    duration: 2500
  })
  fetchData()
}

async function fetchData() {
  try {
    tableLoading.value = true
    const data = (await monomerModelInfo({ monomerId: props.info.id })) || {}
    console.log(data)
    tableData.value = [
      {
        ...data,
        projectName: props.info.projectName,
        monomerName: props.info.name,
        edition: props.info.edition,
        hasModelImport: isNotBlank(data)
      }
    ]
  } catch (error) {
    console.log('单体模型信息', error)
  } finally {
    tableLoading.value = false
  }
}

async function delIt(row) {
  ElMessageBox.confirm(`是否确认删除 “${row.monomer.name}” 单体下的模型`, '提示', {
    confirmButtonText: '确认',
    cancelButtonText: '取消',
    type: 'warning'
  }).then(async () => {
    try {
      await modelDel({ monomerId: row.monomer.id })
      ElNotification({ title: '删除模型成功', type: 'success' })
      fetchData()
    } catch (error) {
      console.log('删除模型失败', error)
    }
  })
}

defineExpose({
  fetchData
})
</script>
