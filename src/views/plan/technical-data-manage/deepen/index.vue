<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length>0">
      <div style="height:60px;">
        <common-button size="small" style="float:right;" type="primary">操作日志</common-button>
      </div>
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
        :stripe="false"
        :span-method="objectSpanMethod"
      >
        <el-table-column key="projectName" prop="projectName" :show-overflow-tooltip="true" label="项目" align="center">
          <template v-slot="scope">
            <el-tooltip :content="scope.row.projectSerialNumber+' '+scope.row.projectName" :disabled="!scope.row.projectName" :show-after="50" placement="top">
              <span>{{scope.row.projectName}}</span>
            </el-tooltip>
          </template>
        </el-table-column>
        <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="单体" align="center">
          <template v-slot="scope">
            <div style="cursor: pointer;">{{ scope.row.name }}</div>
            <el-tag effect="plain">结构</el-tag>
          </template>
        </el-table-column>
        <el-table-column align="center" label="单元">
          <template v-slot="scope">
            <template v-if="scope.row.areaArr.length > 0">
              <template v-for="(k,i) in scope.row.areaArr" :key="k.id">
                <div :class="i===scope.row.areaArr.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                  {{k.name}}
                </div>
              </template>
            </template>
            <div v-else class="sandwich-cell-bottom"></div>
          </template>
        </el-table-column>
        <el-table-column key="model" prop="model" :show-overflow-tooltip="true" label="模型" align="center">
          <template v-slot="scope">
            <common-button size="small" type="primary" @click="uploadModel(scope.row)">导入</common-button>
          </template>
        </el-table-column>
        <el-table-column key="deepen" prop="deepen" :show-overflow-tooltip="true" label="深化图纸" align="center">
          <template v-slot="scope">
            <common-button size="small" type="primary" @click="uploadDeepen(scope.row)">操作</common-button>
          </template>
        </el-table-column>
        <el-table-column key="machPart" prop="machPart" :show-overflow-tooltip="true" label="零件图(DXF格式)" align="center">
          <template v-slot="scope">
            <common-button size="small" type="primary" @click="uploadMachPart(scope.row)">操作</common-button>
          </template>
        </el-table-column>
      </common-table>
      <common-drawer
        ref="deepenRef"
        :show-close="true"
        size="80%"
        title="深化图纸"
        append-to-body
        v-model="deepenVisible"
        :close-on-click-modal="false"
      >
        <template #content>
          <deepenTable :queryMonomerId="queryMonomerId" :currentProject="currentProject"/>
        </template>
      </common-drawer>
      <common-drawer
        ref="drawerRef"
        :show-close="true"
        size="80%"
        title="零件图纸"
        append-to-body
        v-model="machinePartVisible"
        :close-on-click-modal="false"
      >
        <template #content>
          <machinePartTable :queryMonomerId="queryMonomerId" :currentProject="currentProject"/>
        </template>
      </common-drawer>
    </template>
  </div>
</template>

<script setup>
import { monomerAll as getAll } from '@/api/plan/monomer'
import { ref, watch } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import { mapGetters } from '@/store/lib'
import { isNotBlank } from '@data-type/index'
import { TechnologyTypeAllEnum, businessTypeEnum } from '@enum-ms/contract'
import deepenTable from './module/deepen-table'
import machinePartTable from './module/machine-part-table'

const { globalProject } = mapGetters(['globalProject'])

const tableRef = ref()
const loading = ref(false)
const deepenVisible = ref(false)
const machinePartVisible = ref(false)
const queryMonomerId = ref()
const currentProject = ref()
const deepenRef = ref()
const drawerRef = ref()

const { maxHeight } = useMaxHeight({
  wrapperBox: '.deep',
  paginate: true,
  extraHeight: 40
})

const tableData = ref([])
watch(
  () => globalProject.value,
  (val) => {
    if (isNotBlank(val)) {
      const projectContent = []
      val.projectContentList.forEach((v) => {
        if (val.businessType === businessTypeEnum.MACHINING.V) {
          if (v.no) {
            projectContent.push(Number(v.no))
          }
        } else if (val.businessType === businessTypeEnum.INSTALLATION.V) {
          if (v.childrenList && v.childrenList.length > 0) {
            v.childrenList.forEach((value) => {
              if (value.no) {
                projectContent.push(Number(value.no))
              }
            })
          }
        }
      })
      if (projectContent.indexOf(TechnologyTypeAllEnum.STRUCTURE.V) > -1) {
        currentProject.value = val
        fetchData(val.id)
      } else {
        tableData.value = []
        currentProject.value = {}
      }
    } else {
      tableData.value = []
      currentProject.value = {}
    }
  },
  { deep: true, immediate: true }
)

async function fetchData(val) {
  loading.value = true
  try {
    const { content = [] } = (await getAll(val)) || {}
    content.map((v, index) => {
      v.projectName = globalProject.value.name
      v.projectShortName = globalProject.value.shortName
      v.projectSerialNumber = globalProject.value.serialNumber
      v.areaArr = v.areaSimpleList.filter(k => k.productType === TechnologyTypeAllEnum.STRUCTURE.V)
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

function objectSpanMethod({ row, column, rowIndex, columnIndex }) {
  if (columnIndex === 0) {
    return {
      rowspan: row.rowSpanNum,
      colspan: 1
    }
  }
}

function uploadModel(row) {

}
function uploadDeepen(row) {
  deepenVisible.value = true
  queryMonomerId.value = row.id
}
function uploadMachPart(row) {
  machinePartVisible.value = true
  queryMonomerId.value = row.id
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1){
    .cell{
      opacity:0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
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
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 2px;
  }
}
.upload-table {
  ::v-deep(.cell) {
    padding-left: 0;
    padding-right: 0;
  }
  ::v-deep(thead.is-group th) {
    background: #fff;
  }
}
::v-deep(.el-table--small .el-table__cell){
  padding:4px 0;
}
</style>
