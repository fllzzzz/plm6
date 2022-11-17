<template>
  <common-drawer ref="drawerRef" title="生产任务单" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="60%">
    <template #titleAfter>
      <el-tag>项目：{{ props.detailData.project?.name }}</el-tag>
      <el-tag>产线：{{ props.detailData.workshop?.name }}>{{ props.detailData.productionLine?.name }}</el-tag>
      <el-tag>总量：{{ props.detailData.taskQuantity }}/{{ props.detailData.taskMete }}</el-tag>
      <el-tag>任务单号：{{ props.detailData.scheduleOrder }}</el-tag>
    </template>
    <template #content>
      <div class="head-container">
        <div style="float: left">
          <common-radio-button
            v-model="processId"
            :options="processList"
            type="other"
            :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
            size="small"
            showOptionAll
            class="filter-item"
            @change="handleProcessChange"
          />
          <common-radio-button
            v-if="props.detailData.productType === componentTypeEnum.ASSEMBLE.V"
            v-model="type"
            :options="typeEnum.ENUM"
            type="enum"
            size="small"
            class="filter-item"
            @change="handleTypeChange"
          />
        </div>
        <div style="float: right; width: 300px">
          <print-table
            :api-key="
              props.detailData.productType === componentTypeEnum.ASSEMBLE.V ? 'mesAssembleProductionTaskOrder' : 'mesProductionTaskOrder'
            "
            :params="{ ...query }"
            size="mini"
            type="warning"
            class="filter-item"
          />
        </div>
      </div>
      <common-table
        ref="table"
        :data="tableData"
        empty-text="暂无数据"
        :max-height="maxHeight"
        style="width: 100%"
        v-if="props.detailData.productType === componentTypeEnum.ARTIFACT.V"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column :show-overflow-tooltip="true" label="单体" key="monomer.name" prop="monomer.name" align="center" />
        <el-table-column :show-overflow-tooltip="true" label="区域" key="area.name" prop="area.name" align="center" />
        <el-table-column :show-overflow-tooltip="true" label="编号" key="serialNumber" prop="serialNumber" align="center" />
        <el-table-column
          :show-overflow-tooltip="true"
          label="规格"
          key="specification"
          prop="specification"
          align="center"
          min-width="110px"
        />
        <el-table-column :show-overflow-tooltip="true" label="长度（mm）" key="length" prop="length" align="center" />
        <el-table-column :show-overflow-tooltip="true" label="单重（kg）" key="netWeight" prop="netWeight" align="center" />
        <el-table-column :show-overflow-tooltip="true" label="任务数" key="quantity" prop="quantity" align="center" />
        <el-table-column :show-overflow-tooltip="true" label="完成日期" key="complete" prop="completeTime" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.completeTime ? parseTime(scope.row.completeTime, '{y}-{m}-{d}') : '-' }}</span>
          </template>
        </el-table-column>
      </common-table>
      <common-table
        v-if="props.detailData.productType === componentTypeEnum.ASSEMBLE.V && type === typeEnum.TASK_LIST.V"
        ref="table"
        :data="tableData"
        empty-text="暂无数据"
        :max-height="maxHeight"
        style="width: 100%"
      >
        <el-table-column :show-overflow-tooltip="true" label="属性" key="taskType" prop="typeType" align="center">
          <template v-slot="scope">
            <el-tag v-if="scope.row.taskType === componentTypeEnum.ASSEMBLE.V" type="warning">部件</el-tag>
            <el-tag v-else type="success">套料</el-tag>
          </template>
        </el-table-column>
        <el-table-column
          :show-overflow-tooltip="true"
          label="编号"
          key="serialNumber"
          prop="serialNumber"
          align="center"
          min-width="130px"
        />
        <el-table-column :show-overflow-tooltip="true" label="规格" key="specification" prop="specification" align="center" />
        <el-table-column :show-overflow-tooltip="true" label="长度（mm）" key="length" prop="length" align="center" />
        <el-table-column :show-overflow-tooltip="true" label="数量" key="quantity" prop="quantity" align="center" />
        <el-table-column :show-overflow-tooltip="true" label="单重" key="netWeight" prop="netWeight" align="center" />
        <el-table-column :show-overflow-tooltip="true" label="完成日期" key="complete" prop="completeTime" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.completeTime ? parseTime(scope.row.completeTime, '{y}-{m}-{d}') : '-' }}</span>
          </template>
        </el-table-column>
      </common-table>
      <common-table
        v-if="props.detailData.productType === componentTypeEnum.ASSEMBLE.V && type === typeEnum.NESTING_LIST.V"
        ref="tableRef"
        :data="tableData"
        empty-text="暂无数据"
        :max-height="maxHeight"
        style="width: 100%"
        class="upload-table"
      >
        <el-table-column
          :show-overflow-tooltip="true"
          label="套料编号"
          key="serialNumber"
          prop="serialNumber"
          align="center"
          min-width="130px"
        />
        <el-table-column
          :show-overflow-tooltip="true"
          label="材料属性"
          key="typesettingAssembleName"
          prop="typesettingAssembleName"
          align="center"
        >
          <template v-slot="scope">
            <span>{{ scope.row.typesettingAssembleName }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" label="规格" key="specification" prop="specification" align="center" />
        <el-table-column :show-overflow-tooltip="true" label="材质" key="material" prop="material" align="center" />
        <el-table-column :show-overflow-tooltip="true" label="母材长度" key="length" prop="length" align="center" />
        <el-table-column :show-overflow-tooltip="true" label="部件" key="serialNumber" prop="serialNumber" align="center">
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
        <el-table-column :show-overflow-tooltip="true" label="重量" key="weight" prop="weight" align="center">
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
        <el-table-column :show-overflow-tooltip="true" label="数量" key="quantity" prop="quantity" align="center">
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
        <el-table-column :show-overflow-tooltip="true" label="利用长度" key="aLength" prop="aLength" align="center" />
        <el-table-column :show-overflow-tooltip="true" label="损耗率" key="lossRate" prop="lossRate" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.lossRate }}%</span>
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
    </template>
  </common-drawer>
</template>

<script setup>
import { processInfo, getTaskList, getNestingList } from '@/api/mes/work-order-manage/artifact.js'
import { defineProps, defineEmits, ref, computed, watch } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'
import { constantize } from '@/utils/enum/base'
import { parseTime } from '@/utils/date'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
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
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchHook })

const typeEnum = {
  TASK_LIST: { L: '任务清单', K: 'TASK_LIST', V: 1 },
  NESTING_LIST: { L: '套料清单', K: 'NESTING_LIST', V: 2 }
}
constantize(typeEnum)
// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.head-container'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const tableData = ref([])
const processList = ref([])
const processId = ref()
const type = ref(typeEnum.TASK_LIST.V)
const params = computed(() => {
  return {
    orderId: props.detailData.orderId,
    processId: processId.value,
    productType: props.detailData.productType,
    projectId: props.detailData.projectId
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
      productionLineId: props.detailData.productionLine.id
    })
    processList.value = data
    handleProcessChange()
  } catch (error) {
    console.log('获取工序', error)
  }
}

// 构件查看、部件任务清单接口
async function fetch() {
  let _list = []
  try {
    const query =
      props.detailData.productType === componentTypeEnum.ARTIFACT.V ? { ...params.value } : { ...params.value, type: typeEnum.TASK_LIST.V }
    const { content = [], totalElements } = await getTaskList({
      ...query,
      ...queryPage
    })
    setTotalPage(totalElements)
    _list = content
  } catch (err) {
    console.log('获取生产任务单', err)
  } finally {
    tableData.value = _list
  }
}

// 部件套料清单
async function assembleListGet() {
  let _content = []
  try {
    const { content = [], totalElements } = await getNestingList({
      ...params.value,
      type: typeEnum.NESTING_LIST.V,
      ...queryPage
    })
    setTotalPage(totalElements)
    _content = content
  } catch (err) {
    console.log('获取部件套料清单', err)
  } finally {
    tableData.value = _content
  }
}
function handleTypeChange(val) {
  if (val === typeEnum.NESTING_LIST.V) {
    assembleListGet()
  } else {
    fetch()
  }
}

function handleProcessChange(val) {
  if (type.value === typeEnum.NESTING_LIST.V) {
    assembleListGet()
  } else {
    fetch()
  }
}

function fetchHook() {
  if (type.value === typeEnum.NESTING_LIST.V) {
    assembleListGet()
  } else {
    fetch()
  }
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
