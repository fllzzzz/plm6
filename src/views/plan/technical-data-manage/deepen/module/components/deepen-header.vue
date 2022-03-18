<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        class="filter-item"
        :default="false"
        :defaultValue="queryMonomerId"
        @change="crud.toQuery"
        @getCurrentInfo="handleCurrent"
      />
      <common-radio-button
        v-model="query.productType"
        :options="deepenTypeEnum.ENUM"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
    </div>
    <crudOperation>
      <template #optLeft>
        <upload-btn
          ref="deepenRef"
          v-if="query.monomerId && checkPermission(crud.permission.import)"
          :upload-fun="upload"
          :data="carryParam"
          :btn-name="'文件上传'"
          :btn-type="'warning'"
          :btn-size="'mini'"
          :icon="'el-icon-upload'"
          :accept="'.zip'"
          class="filter-item"
          :data-type="query.dataType"
          tip=".dwg,.pdf"
          :material-type="crud.query.type"
          @success="crud.toQuery"
        />
      </template>
      <template #viewLeft>
        <common-button
          v-if="query.monomerId && checkPermission(crud.permission.download)"
          :loading="downloadLoading"
          type="warning"
          icon="el-icon-download"
          size="mini"
          :disabled="crud.data.length===0"
          @click="downloadAll()"
        >下载单体下所有图纸</common-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref, defineEmits, computed } from 'vue'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import { deepenTypeEnum, technicalDataTypeEnum } from '@enum-ms/plan'
import { upload, downloadByMonomer } from '@/api/plan/technical-data-manage/deepen'
import { fileDownload } from '@/utils/file'
import uploadBtn from '../../../components/drawing-upload-btn'
import checkPermission from '@/utils/system/check-permission'

const defaultQuery = {
  dataType: { value: technicalDataTypeEnum.DEEPEN.V, resetAble: false },
  monomerId: { value: undefined, resetAble: false },
  productType: { value: deepenTypeEnum.ARTIFACT.V, resetAble: false }
}

const emit = defineEmits(['currentChange', 'handleUpload'])
const monomerSelectRef = ref()
const downloadLoading = ref(false)
const { crud, query } = regHeader(defaultQuery)
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  queryMonomerId: {
    type: [Number, String],
    default: undefined
  }
})

async function downloadAll() {
  try {
    downloadLoading.value = true
    await fileDownload(downloadByMonomer, { projectId: crud.query.projectId, dataType: crud.query.dataType, monomerId: crud.query.monomerId, productType: crud.query.productType })
  } catch (error) {
    console.log('根据单体下载', error)
  } finally {
    downloadLoading.value = false
  }
}

function handleCurrent(val) {
  emit('currentChange', val)
}

const carryParam = computed(() => {
  return { projectId: props.projectId, monomerId: crud.query.monomerId, dataType: crud.query.dataType, productType: crud.query.productType }
})
</script>
