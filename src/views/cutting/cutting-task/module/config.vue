<template>
  <common-drawer
    ref="drawerRef"
    v-model="drawerVisible"
    direction="rtl"
    size="75%"
    :title="IssueMac.plateState === 1 ? '查看分配量' : '查看下发量'"
    :wrapper-closable="false"
    :before-close="handleClose"
  >
    <template #titleRight>
      <common-button v-if="IssueMac.plateState === 1 && IssueMac.type === 0" size="mini" type="success" @click="Issue">下 发</common-button>
      <common-button v-if="IssueMac.plateState === 1 && IssueMac.type === 1" size="mini" type="danger" @click="clearClick">
        清 除
      </common-button>
      <common-button
        :disabled="changeData.length <= 0"
        v-if="IssueMac.plateState === 1 && IssueMac.type === 2"
        size="mini"
        type="primary"
        @click="changeTaskClick"
      >
        转 产
      </common-button>
    </template>
    <template #content>
      <div class="class-unit-config">
        <div class="query-content">
          <template v-if="!(IssueMac.plateState === 1 && IssueMac.type === 2)">
            <el-input
              v-model="projectName"
              placeholder="请输入项目名称"
              class="filter-item"
              style="width: 200px; margin-bottom: 15px"
              size="small"
              clearable
            />
            <el-input
              v-model="thick"
              placeholder="厚度"
              class="filter-item"
              style="width: 200px; margin-bottom: 15px"
              size="small"
              clearable
            />
            <el-input
              v-model="plateType"
              placeholder="请输入物料种类"
              class="filter-item"
              style="width: 200px; margin-bottom: 15px"
              size="small"
              clearable
            />
            <el-date-picker @change="changeDate" v-model="date" type="date" size="small" class="filter-item" placeholder="请选择日期" />
            <common-button style="float: right" class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left">
              重置
            </common-button>
            <common-button @click="searchClick" style="float: right" class="filter-item" size="mini" type="success" icon="el-icon-search">
              搜索
            </common-button>
          </template>
          <common-select
            style="margin-left: 10px; margin-bottom: 15px"
            v-if="IssueMac.plateState === 1 && IssueMac.type === 2"
            v-model="areaValue"
            :options="machineData"
            type="other"
            :width="500"
            :dataStructure="typeProp"
            size="small"
            placeholder="请选择转产机器"
            class="filter-item"
          />
        </div>
      </div>
      <common-table
        @selection-change="handleSelectionChange"
        ref="tableRef"
        border
        :data="IssueData"
        :highlight-current-row="false"
        row-key="id"
      >
        <el-table-column v-if="IssueMac.plateState === 1" type="selection" align="center" width="55" />
        <el-table-column key="index" type="index" label="序号" align="center" width="60" />
        <el-table-column key="projectName" prop="projectName" :show-overflow-tooltip="true" label="项目/单体" min-width="70">
          <template v-slot="scope">
            <span>{{ scope.row.projectName }}</span>
          </template>
        </el-table-column>
        <el-table-column key="plateType" prop="plateType" :show-overflow-tooltip="true" label="物料种类" min-width="70">
          <template v-slot="scope">
            <span>{{ scope.row.plateType }}</span>
          </template>
        </el-table-column>
        <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="70">
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column key="thick" prop="thick" :show-overflow-tooltip="true" label="厚(mm)" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.thick }}</span>
          </template>
        </el-table-column>
        <el-table-column key="width" prop="width" :show-overflow-tooltip="true" label="宽(mm)" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.width }}</span>
          </template>
        </el-table-column>
        <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="长(mm)" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.length }}</span>
          </template>
        </el-table-column>
        <el-table-column key="plateState" prop="plateState" :show-overflow-tooltip="true" label="状态" min-width="60">
          <template v-slot="scope">
            <span>{{ steelPlateEnum.VL[scope.row.plateState] }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, ref } from 'vue'
import { get } from '@/api/cutting/project-data'
import { steelPlateEnum } from '@enum-ms/cutting'
import useVisible from '@compos/use-visible'
import { ElMessage } from 'element-plus'
import { cleanTask, changeTask, sentTask } from '@/api/cutting/machine'

const emit = defineEmits(['updateChange'])

const typeProp = { key: 'id', label: 'machineName', value: 'mac' }

const searchObj = {
  projectName: undefined,
  thick: undefined,
  plateType: undefined,
  importTime: undefined
}

const areaValue = ref()

const props = defineProps(
  {
    visible: { type: Boolean, default: false },
    IssueMac: { type: Object, require: true },
    machineData: { type: Object, require: true }
  }
)

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })
const drawerRef = ref()
const date = ref('')
const IssueData = ref([])
const changeData = ref([])

function changeDate() {
  const a = new Date(date.value.getTime()).getFullYear()
  const b = new Date(date.value.getTime()).getMonth() + 1
  const c = new Date(date.value.getTime()).getDate()
  searchObj.importTime = a + '-' + b + '-' + c
}

// 页面数据
async function getIssueMac() {
  try {
    const { content } = await get({ mac: props.IssueMac.mac, plateState: props.IssueMac.plateState })
    IssueData.value = content
  } catch (err) {
    console.log(err)
  }
}

function handleSelectionChange(val) {
  changeData.value = val
}

function showHook() {
  if (props.IssueMac !== '') {
    getIssueMac()
  }
}

async function Issue() {
  try {
    const list = changeData.value.map(item => { return item.id })
    const message = await sentTask(list)
    ElMessage({ message: message, type: 'success' })
    getIssueMac()
    emit('updateChange')
  } catch (err) {
    console.log(err)
  }
}
function searchClick() {
  console.log('searchObj', searchObj)
}
async function clearClick() {
  try {
    const list = changeData.value.map(item => { return item.id })
    const message = await cleanTask({ mac: props.IssueMac.mac }, list)
    ElMessage({ message: message, type: 'success' })
    getIssueMac()
    emit('updateChange')
  } catch (err) {
    console.log(err)
  }
}

async function changeTaskClick() {
  if (areaValue.value === undefined) {
    ElMessage({ message: '请选择转产机器', type: 'error' })
  } else {
    const list = changeData.value.map(item => { return item.id })
    try {
      const message = await changeTask({ mac: areaValue.value }, list)
      ElMessage({ message: message, type: 'success' })
      getIssueMac()
      emit('updateChange')
    } catch (err) {
      console.log(err)
    }
  }
}

</script>

<style lang="scss" scoped>
$default-cell-mask-color: #52f09840;
::v-deep(.mask-td) {
  .cell {
    &:after {
      background-color: $default-cell-mask-color;
    }
  }
}

.filter-item {
  margin-right: 5px;
}

.classify-name {
  padding: 0 10px;
}

::v-deep(.el-table) {
  th,
  td {
    padding: 0;
  }
  .el-tooltip {
    line-height: 40px;
  }
  .cell {
    line-height: 32px;
    padding: 0;
  }
  th .cell {
    padding: 0 10px;
  }
  td:first-child .cell {
    padding: 0;
  }
  .el-table__body .el-input__inner,
  .el-table__body .el-textarea__inner {
    border-radius: 0;
  }

  .cell {
    .el-input__inner {
      border: none;
    }
    .el-input-number__increase {
      border-left: none;
      margin-right: 10px;
    }
    .el-input-number__decrease {
      border-right: none;
      margin-left: 10px;
    }
  }
}
</style>
