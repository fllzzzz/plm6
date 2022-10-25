<template>
  <common-drawer ref="drawerRef" title="生产任务单" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="60%">
    <template #titleAfter>
      <el-tag>项目：{{ props.detailData.serialNumber }}-{{ props.detailData.projectName }}</el-tag>
      <el-tag>产线：{{ props.detailData.workshopName }}>{{ props.detailData.productionLineName }}</el-tag>
      <el-tag>总量：{{ props.detailData.taskQuantity }}/{{ props.detailData.taskMete }}</el-tag>
      <el-tag>任务单号：{{ props.detailData.scheduleOrder }}</el-tag>
    </template>
    <template #content>
      <common-radio-button
        v-model="processId"
        :options="processList"
        type="other"
        :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
        size="small"
        showOptionAll
        class="filter-item"
        style="margin-bottom: 8px"
        @change="handleProcessChange"
      />
      <common-radio-button
        v-if="props.detailData.productType === componentTypeEnum.ASSEMBLE.V"
        v-model="type"
        :options="typeEnum.ENUM"
        type="enum"
        size="small"
        class="filter-item"
        style="margin: 0 0 8px 8px"
        @change="handleTypeChange"
      />
      <common-table
        ref="table"
        :data="tableData.taskList"
        empty-text="暂无数据"
        :max-height="maxHeight"
        style="width: 100%"
        v-if="props.detailData.productType === componentTypeEnum.ARTIFACT.V"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column label="单体" key="monomerName" prop="monomerName" align="center" />
        <el-table-column label="区域" key="areaName" prop="areaName" align="center" />
        <el-table-column label="编号" key="serialNumber" prop="serialNumber" align="center" />
        <el-table-column label="规格" key="specification" prop="specification" align="center" min-width="110px"/>
        <el-table-column label="长度（mm）" key="length" prop="length" align="center" />
        <el-table-column label="单重（kg）" key="netWeight" prop="netWeight" align="center" />
        <el-table-column label="任务数" key="quantity" prop="quantity" align="center" />
        <el-table-column label="完成日期" key="complete" prop="completeTime" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.completeTime ? parseTime(scope.row.completeTime, '{y}/{m}/{d}') : '-' }}</span>
          </template>
        </el-table-column>
      </common-table>
      <common-table
        ref="table"
        :data="tableData.taskList"
        empty-text="暂无数据"
        :max-height="maxHeight"
        style="width: 100%"
        v-if="props.detailData.productType === componentTypeEnum.ASSEMBLE.V && type === typeEnum.TASK_LIST.V"
      >
        <el-table-column label="属性" key="taskType" prop="typeType" align="center">
          <template v-slot="scope">
            <el-tag v-if="scope.row.taskType === componentTypeEnum.ASSEMBLE.V" type="warning">部件</el-tag>
            <el-tag v-else type="success">套料</el-tag>
          </template>
        </el-table-column>
        <el-table-column label="编号" key="serialNumber" prop="serialNumber" align="center" min-width="130px" />
        <el-table-column label="规格" key="specification" prop="specification" align="center" />
        <el-table-column label="长度（mm）" key="length" prop="length" align="center" />
        <el-table-column label="数量" key="quantity" prop="quantity" align="center" />
        <el-table-column label="单重" key="netWeight" prop="netWeight" align="center" />
        <el-table-column label="完成日期" key="complete" prop="completeTime" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.completeTime ? parseTime(scope.row.completeTime, '{y}/{m}/{d}') : '-' }}</span>
          </template>
        </el-table-column>
      </common-table>
      <common-table
        ref="table"
        :data="tableData.typesettingList"
        empty-text="暂无数据"
        :max-height="maxHeight"
        style="width: 100%"
        class="upload-table"
        v-if="props.detailData.productType === componentTypeEnum.ASSEMBLE.V && type === typeEnum.MATERIAL_LIST.V"
      >
        <el-table-column label="套料编号" key="serialNumber" prop="serialNumber" align="center" min-width="130px" />
        <el-table-column label="材料属性" key="typesettingAssembleName" prop="typesettingAssembleName" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.typesettingAssembleName }}</span>
          </template>
        </el-table-column>
        <el-table-column label="规格" key="specification" prop="specification" align="center" />
        <el-table-column label="材质" key="material" prop="material" align="center" />
        <el-table-column label="母材长度" key="length" prop="length" align="center" />
        <el-table-column label="部件" key="serialNumber" prop="serialNumber" align="center">
          <template v-slot="scope">
            <div
              v-for="(item, index) in scope.row.assembleList"
              :key="item"
              :class="index === scope.row.assembleList.length - 1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'"
            >
              <span>{{ item.serialNumber }}</span>
            </div>
          </template>
        </el-table-column>
        <el-table-column label="重量" key="weight" prop="weight" align="center">
          <template v-slot="scope">
            <div
              v-for="(item, index) in scope.row.assembleList"
              :key="item"
              :class="index === scope.row.assembleList.length - 1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'"
            >
              <span>{{ item.weight }}</span>
            </div>
          </template>
        </el-table-column>
        <el-table-column label="数量" key="quantity" prop="quantity" align="center">
          <template v-slot="scope">
            <div
              v-for="(item, index) in scope.row.assembleList"
              :key="item"
              :class="index === scope.row.assembleList.length - 1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'"
            >
              <span>{{ item.quantity }}</span>
            </div>
          </template>
        </el-table-column>
        <el-table-column label="利用长度" key="aLength" prop="aLength" align="center" />
        <el-table-column label="利用率" key="lossRate" prop="lossRate" align="center" />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { processInfo, productTask } from '@/api/mes/work-order-manage/artifact.js'
import { defineProps, defineEmits, ref, computed, watch } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'
import { constantize } from '@/utils/enum/base'
import { parseTime } from '@/utils/date'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailData: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

const typeEnum = {
  TASK_LIST: { L: '任务清单', K: 'TASK_LIST', V: 1 },
  MATERIAL_LIST: { L: '材料清单', K: 'MATERIAL_LIST', V: 2 }
}
constantize(typeEnum)
// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const tableData = ref({})
const tableLoading = ref(false)
const processList = ref([])
const processId = ref()
const type = ref(typeEnum.TASK_LIST.V)
const params = computed(() => {
  return {
    orderId: props.detailData.orderId,
    processId: processId.value,
    productType: props.detailData.productType
  }
})

watch(
  () => drawerVisible.value,
  (val) => {
    if (val) {
      processId.value = undefined
      processGet()
    }
  },
  { deep: true, immediate: true }
)

async function processGet() {
  try {
    const data = await processInfo({
      orderId: props.detailData.orderId,
      productionLineId: props.detailData.productionLineId
    })
    processList.value = data
    handleProcessChange()
  } catch (error) {
    console.log('获取工序', error)
  }
}
async function fetch() {
  try {
    tableLoading.value = true
    const query =
      props.detailData.productType === componentTypeEnum.ARTIFACT.V ? { ...params.value } : { ...params.value, type: type.value }
    const data = await productTask({
      ...query
    })
    tableData.value = data
  } catch (err) {
    console.log('获取生产任务单', err)
  } finally {
    tableLoading.value = false
  }
}
function handleTypeChange(val) {
  fetch()
}

function handleProcessChange(val) {
  fetch()
}
</script>
<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #eef7ea;
}
::v-deep(.blue-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1) {
    .cell {
      opacity: 0;
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
.sandwich-cell-top,
.sandwich-cell-bottom {
  padding: 5px;
  min-height: 40px;
  line-height: 30px;
  box-sizing: border-box;
  overflow: hidden;
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 2px;
  }
}
.sandwich-cell-top {
  border-bottom: 1px solid #dfe6ec;
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
::v-deep(.el-table--small .el-table__cell) {
  padding: 4px 0;
}
.float-ele {
  float: left;
}
.div-ellipsis {
  width: 100%;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
</style>
