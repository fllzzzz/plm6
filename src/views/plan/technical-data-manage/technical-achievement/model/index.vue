<template>
  <div class="app-container">
    <!--表格渲染-->
    <common-table
      v-loading="loading"
      ref="tableRef"
      :data="tableData"
      :empty-text="'暂无数据'"
      :max-height="maxHeight"
      style="width: 100%"
      :loading="loading"
      class="upload-table"
      return-source-data
      :showEmptySymbol="false"
    >
      <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="单体" align="center">
        <template v-slot="scope">
          <div style="margin: 2px 0 6px 0">
            <div>{{ scope.row.name }}</div>
            <el-tag effect="plain">{{globalProject.projectType===projectTypeEnum.STEEL.V?'结构':'分段'}}</el-tag>
          </div>
        </template>
      </el-table-column>
      <el-table-column align="center" label="单元">
        <template v-slot="scope">
          <template v-if="scope.row.areaArr.length > 0">
            <div v-for="(k, i) in scope.row.areaArr" :key="k.id">
              <div :class="i === scope.row.areaArr.length - 1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'">
                {{ k.name }}
              </div>
            </div>
          </template>
          <div v-else class="sandwich-cell-bottom"></div>
        </template>
      </el-table-column>
      <el-table-column key="importMode" prop="importMode" show-overflow-tooltip label="上传模式" align="center">
        <template v-slot="scope">
          <el-tag v-if="scope.row.importMode" size="medium" effect="plain">{{ modelImportModeEnum.VL?.[scope.row.importMode] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column key="edition" prop="edition" show-overflow-tooltip label="模型版本" align="center">
        <template v-slot="scope">
          <el-tag v-if="scope.row.edition" type="success" size="medium" effect="plain">{{ bimTeklaEditionEnum.VL?.[scope.row.edition] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column key="successQuantity" prop="successQuantity" show-overflow-tooltip label="转换成功次数" align="center"/>
      <el-table-column key="model" prop="model" :show-overflow-tooltip="true" label="操作" align="center" width="130">
        <template v-slot="scope">
          <common-button v-permission="permission.import" size="mini" type="warning" @click="configModel(scope.row)">配置</common-button>
          <common-button v-permission="permission.detail" :disabled="!scope.row.importMode || !scope.row.edition" type="primary" size="mini" icon="el-icon-document" @click="uploadModel(scope.row)" />
        </template>
      </el-table-column>
    </common-table>
    <common-drawer
      ref="drawerRef"
      show-close
      size="60%"
      :title="`模型${currentRow.importMode ? '【' + modelImportModeEnum.VL[currentRow.importMode] + '】' : ''}`"
      append-to-body
      v-model="modelVisible"
    >
      <template #titleAfter>
        <el-tag v-if="currentRow.projectShortName" size="medium" effect="plain">{{ currentRow.projectShortName }}</el-tag>
        <el-tag v-if="currentRow.name" size="medium" effect="plain">{{ currentRow.name }}</el-tag>
        <el-tag v-if="currentRow.edition" type="warning" size="medium" effect="plain">{{ bimTeklaEditionEnum.VL?.[currentRow.edition] }}</el-tag>
      </template>
      <template #content>
        <component
          ref="modelContentRef"
          :is="currentView"
          :info="currentRow"
          :project-id="globalProject.id"
          @close="modelVisible = false"
        ></component>
      </template>
    </common-drawer>
    <model-config-form v-model:visible="modelConfigVisible" :info="currentRow" @success="fetchData" />
  </div>
</template>

<script setup>
import { monomerAll as getAll } from '@/api/plan/monomer'
import { ref, watch, computed, nextTick } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import { mapGetters } from '@/store/lib'
import { isNotBlank } from '@data-type/index'
import { TechnologyTypeAllEnum, projectTypeEnum } from '@enum-ms/contract'
import { modelImportModeEnum, bimTeklaEditionEnum } from '@enum-ms/bim'
import { modelFileListPM as permission } from '@/page-permission/plan'

import modelConfigForm from './module/model-config-form'
import modelMonomerMode from './module/model-monomer-mode'
import modelIntegrateMode from './module/model-integrate-mode'

const { globalProject } = mapGetters(['globalProject'])

const tableRef = ref()
const loading = ref(false)
const modelConfigVisible = ref(false)
const modelVisible = ref(false)
const currentRow = ref({})
const drawerRef = ref()
const modelContentRef = ref()

const { maxHeight } = useMaxHeight()

const tableData = ref([])
watch(
  () => globalProject.value,
  (val) => {
    tableData.value = []
    if (isNotBlank(val)) {
      if (val.projectContentList.some(v => Number(v.no) === TechnologyTypeAllEnum.STRUCTURE.V || Number(v.no) === TechnologyTypeAllEnum.BRIDGE.V)) {
        fetchData(val.id)
      }
    }
  },
  { deep: true, immediate: true }
)

const currentView = computed(() => {
  return currentRow.value.importMode === modelImportModeEnum.INTEGRATION.V ? modelIntegrateMode : modelMonomerMode
})

async function fetchData(val = globalProject.value?.id) {
  loading.value = true
  try {
    const { content = [] } = (await getAll(val)) || {}
    content.map((v, index) => {
      v.projectName = globalProject.value.name
      v.projectShortName = globalProject.value.shortName
      v.projectSerialNumber = globalProject.value.serialNumber
      v.edition = v.bimConfig?.edition
      v.importMode = v.bimConfig?.importMode
      v.areaArr = v.areaSimpleList.filter((k) => k.productType === TechnologyTypeAllEnum.STRUCTURE.V || k.productType === TechnologyTypeAllEnum.BRIDGE.V)
      if (index === 0) {
        v.rowSpanNum = content.length
      }
    })
    tableData.value = content
    loading.value = false
  } catch (error) {
    console.log('获取单体列表', error)
    loading.value = false
  }
}

function configModel(row) {
  currentRow.value = Object.assign({}, row)
  modelConfigVisible.value = true
}

function uploadModel(row) {
  currentRow.value = Object.assign({}, row)
  modelVisible.value = true
  nextTick(() => {
    modelContentRef.value?.fetchData()
  })
}

</script>

<style lang="scss" scoped>
.sandwich-cell-top {
  border-bottom: 1px solid #dfe6ec;
}
.sandwich-cell-top,
.sandwich-cell-bottom {
  padding: 5px;
  height: 40px;
  line-height: 30px;
  box-sizing: border-box;
  overflow: hidden;
}
.upload-table {
  ::v-deep(.cell) {
    padding-left: 0;
    padding-right: 0;
  }
  ::v-deep(.el-table__body-wrapper .el-table__cell) {
    padding: 0;
  }
}
</style>
