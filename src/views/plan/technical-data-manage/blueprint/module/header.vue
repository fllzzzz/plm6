<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        :default="false"
        clearable
        class="filter-item"
        @change="crud.toQuery"
        @getCurrentInfo="handleCurrent"
      />
      <el-input
        v-model="query.fileName"
        placeholder="输入文件名搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
      />
      <rrOperation/>
    </div>
    <crudOperation :permission="crud.permission">
      <template #optLeft>
        <common-button type="warning" size="mini" @click="emit('handleUpload')" v-permission="crud.permission.import" class="filter-item">
          文件导入
        </common-button>
      </template>
      <template #viewLeft>
        <common-button
          v-if="projectId && checkPermission(crud.permission.download)"
          :loading="downloadLoading"
          type="warning"
          icon="el-icon-download"
          size="mini"
          :disabled="!projectId || crud.data.length===0"
          @click="downloadAll()"
        >{{query.monomerId?'下载本单体下所有文件':'下载项目下所有文件'}}</common-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref, defineEmits } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { technicalDataTypeEnum } from '@enum-ms/plan'
import { downloadByMonomer } from '@/api/plan/technical-data-manage/deepen'
import monomerSelect from '@/components-system/plan/monomer-select'
import { fileDownload } from '@/utils/file'
import checkPermission from '@/utils/system/check-permission'

const defaultQuery = {
  monomerId: undefined,
  dataType: technicalDataTypeEnum.BLUEPRINT.V// 类型 1蓝图 2变更文件 3模型 4其他文件
}

const emit = defineEmits(['currentChange', 'handleUpload'])
const { crud, query } = regHeader(defaultQuery)
const downloadLoading = ref(false)
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})
async function downloadAll() {
  try {
    downloadLoading.value = true
    await fileDownload(downloadByMonomer, { projectId: crud.query.projectId, dataType: crud.query.dataType, monomerId: crud.query.monomerId })
  } catch (error) {
    console.log('根据单体下载', error)
  } finally {
    downloadLoading.value = false
  }
}

function handleCurrent(val) {
  emit('currentChange', val)
}
// function handleCurrent(val) {
//   currentMonomer.value = val
// }
</script>
