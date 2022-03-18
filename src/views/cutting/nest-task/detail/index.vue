<template>
  <common-dialog
    top="10vh"
    :title="'所属项目：' + titleName"
    width="700px"
    direction="rtl"
    ref="drawerRef"
    fullscreen
    :show-close="false"
    :close-on-click-modal="false"
    custom-class="section-steel-detail"
    v-model="drawerVisible"
    :before-close="handleClose"
  >
    <template #titleLeft>
      <common-select
        style="margin-right: 10px"
        v-model="monomerValue"
        :options="monomerList"
        type="other"
        :dataStructure="typeProp"
        size="small"
        placeholder="请选择单体"
        class="filter-item"
        @change="monomerChange"
      />
      <common-select
        style="margin-right: 10px"
        v-if="monomerValue !== undefined"
        v-model="areaValue"
        :options="areaList"
        type="other"
        :dataStructure="typeProp"
        size="small"
        placeholder="请选择单元"
        class="filter-item"
        @change="areaChange"
      />
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery()">
        重置
      </common-button>
    </template>
    <template #titleRight>
      <common-button :disabled="!(handleSelection.length !== 0)" type="primary" size="mini" @click="createTask(1)">投料操作</common-button>
      <common-button size="mini" @click="handleClose">关 闭</common-button>
    </template>
    <div class="flex-rss">
      <common-table
        row-key="id"
        ref="tableRef"
        :max-height="200"
        style="width: 100%"
        v-loading="loadingPart"
        :data="partData"
        @selection-change="handleSelectionChange"
        :span-method="spanMethod"
      >
        <el-table-column type="selection" align="center" width="55" />
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="monomerName" prop="monomerName" :show-overflow-tooltip="true" label="单体" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.monomerName }}</span>
          </template>
        </el-table-column>
        <el-table-column key="areaName" prop="areaName" :show-overflow-tooltip="true" label="单元" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.areaName }}</span>
          </template>
        </el-table-column>
        <el-table-column key="sum" prop="sum" :show-overflow-tooltip="true" label="零件数量（件）" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.sum }}</span>
          </template>
        </el-table-column>
        <el-table-column key="reduce" prop="reduce" :show-overflow-tooltip="true" label="零件重量（kg）" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.reduce }}</span>
          </template>
        </el-table-column>
        <el-table-column key="packNum" prop="packNum" :show-overflow-tooltip="true" label="已创建数（件）" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.packNum }}</span>
          </template>
        </el-table-column>
        <el-table-column key="packWeight" prop="packWeight" :show-overflow-tooltip="true" label="已创建量（kg）" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.packWeight }}</span>
          </template>
        </el-table-column>
        <!-- 操作 -->
        <el-table-column align="center" label="操作" min-width="60">
          <template v-slot="scope">
            <common-button size="mini" type="primary" icon="el-icon-view" @click="details(scope.row)">查看</common-button>
          </template>
        </el-table-column>
      </common-table>
    </div>
    <el-divider></el-divider>
    <div class="TaskPackage">
      <div v-if="createNum !== 1" class="taskTabHed head-container">
        <group
          style="margin-right: 8px"
          :value="'厚度'"
          v-model="radioButton"
          :options="radioButtonData"
          show-option-all
          size="small"
          @change="radioButtonChange"
        />
        <group
          style="margin-right: 8px"
          :value="'材质'"
          v-model="materialButton"
          :options="material"
          show-option-all
          size="small"
          @change="radioButtonChange"
        />
        <common-radio-button
          style="margin-right: 8px"
          class="enumBtn"
          v-model="PlateType"
          :options="PlateTypeEnum.ENUM"
          show-option-all
          type="enum"
          size="small"
          @change="radioButtonChange"
        />
        <common-button size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="Query()"> 重置 </common-button>
        <common-button style="float: right" :disabled="workListTask" type="success" size="mini" @click="nestWorkListClick">
          创建套料工单
        </common-button>
      </div>
      <common-table
        @selection-change="handleTaskChange"
        v-loading="loadingPackage"
        ref="tableRef"
        :data="taskPackage"
        :max-height="500"
        style="width: 100%"
        row-key="id"
      >
        <el-table-column type="selection" align="center" width="55" />
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="monomerName" prop="monomerName" :show-overflow-tooltip="true" label="单体" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.monomerName }}</span>
          </template>
        </el-table-column>
        <el-table-column key="areaName" prop="areaName" :show-overflow-tooltip="true" label="区域" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.areaName }}</span>
          </template>
        </el-table-column>
        <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
            <el-tag style="float: right; margin-right: 10px" v-if="scope.row.relationType && scope.row.relationType === 2" type="success">
              零件板
            </el-tag>
            <el-tag
              style="float: right; margin-right: 10px"
              v-else-if="scope.row.relationType && scope.row.relationType === 16"
              type="danger"
            >
              翼腹板
            </el-tag>
          </template>
        </el-table-column>
        <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="长度" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.length }}</span>
          </template>
        </el-table-column>
        <el-table-column key="thick" prop="thick" :show-overflow-tooltip="true" label="厚度" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.thick }}</span>
          </template>
        </el-table-column>
        <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column key="netWeight" prop="netWeight" :show-overflow-tooltip="true" label="单重（kg）" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.netWeight }}</span>
          </template>
        </el-table-column>
        <el-table-column key="totalNetWeight" prop="totalNetWeight" :show-overflow-tooltip="true" label="总重（kg）" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.totalNetWeight }}</span>
          </template>
        </el-table-column>
      </common-table>
    </div>
  </common-dialog>

  <!-- 零件工单 -->
  <common-dialog width="60%" title="零件清单" append-to-body v-model="innerVisible">
    <common-table ref="tableRef" :data="PartByCutTaskIdData" :max-height="400" style="width: 100%" row-key="id">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column key="monomerName" prop="monomerName" :show-overflow-tooltip="true" label="单体" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.monomerName }}</span>
        </template>
      </el-table-column>
      <el-table-column key="areaName" prop="areaName" :show-overflow-tooltip="true" label="区域" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.areaName }}</span>
        </template>
      </el-table-column>
      <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="长度" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.length }}</span>
        </template>
      </el-table-column>
      <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column key="netWeight" prop="netWeight" :show-overflow-tooltip="true" label="单重（kg）" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.netWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column key="totalNetWeight" prop="totalNetWeight" :show-overflow-tooltip="true" label="总重（kg）" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.totalNetWeight }}</span>
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import useVisible from '@compos/use-visible'
import { defineProps, defineEmits, ref } from 'vue'
import group from './group.vue'
import { ElMessage } from 'element-plus'
import { getTaskByProjectId, feedingOperation, ByAreaId, area, monomer, createOrder } from '@/api/cutting/radan-controller'
import { PlateTypeEnum } from '@enum-ms/cutting'

const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
  detailData: {
    type: Object,
    required: true
  }
})

const emit = defineEmits(['update:visible'])
const partData = ref([]) // 零件数据

const handleSelection = ref([]) // 选中数据
const workSelectList = ref([]) // 选中数据

const areaIdList = ref([])
const taskPackage = ref([]) // 任务包数据
const PartByCutTaskIdData = ref([]) // 零件清单
const material = ref([])
const innerVisible = ref(false)
const workListTask = ref(true)// 创建工单操作禁用
const titleName = ref('')
const loadingPart = ref(false)
const loadingPackage = ref(false)

const areaValue = ref()
const monomerValue = ref()
const areaList = ref([])// 区域
const monomerList = ref([])// 单体

const radioButtonData = ref([])
const radioButton = ref() // 厚度筛选
const materialButton = ref() // 材质筛选

const PlateType = ref() // 零件板 翼腹板
const createNum = ref(1)

const typeProp = { key: 'id', label: 'name', value: 'id' }

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', closeHook, showHook })

async function CutPart(projectId, monomerId, areaId) {
  loadingPart.value = true
  try {
    const data = await getTaskByProjectId({ projectId: props.detailData.projectId, monomerId: monomerId, areaId: areaId })
    partData.value = data
    titleName.value = data[0].projectName
  } catch (err) {
    console.log(err)
  }
  loadingPart.value = false
}

async function getByCutTaskId(params) {
  const { content } = await ByAreaId(params)
  PartByCutTaskIdData.value = content
}

function handleSelectionChange(val) {
  handleSelection.value = val
  console.log('handleSelection.value', handleSelection.value)
}

async function radioButtonChange(val) {
  createTask()
}

function handleTaskChange(val) {
  if (val.length > 0) {
    workListTask.value = false
    workSelectList.value = val
  } else {
    workListTask.value = true
    workSelectList.value = []
  }
}

function showHook() {
  if (props.detailData) {
    CutPart() // 1
    getMonomer()// 单体
  }
}

function closeHook() {
  partData.value = []
  handleSelection.value = [] // 12312321
  monomerValue.value = undefined
  createNum.value = 1
  material.value = []
  taskPackage.value = []
  radioButtonData.value = []
}

async function getMonomer() {
  try {
    const { content } = await monomer({ projectId: props.detailData.projectId, productType: 1 })
    monomerList.value = content
  } catch (err) {
    console.log(err)
  }
}

async function getArea(projectId, monomerId) {
  try {
    const { content } = await area({ projectId: projectId, monomerId: monomerId, productType: 1 })
    areaList.value = content
  } catch (err) {
    console.log(err)
  }
}

async function createTask(Str) {
  createNum.value = Str
  loadingPackage.value = true
  try {
    areaIdList.value = []
    areaIdList.value = unique(handleSelection.value.map((item) => { return item.areaId }))
    const { content } = await feedingOperation(areaIdList.value, { projectId: props.detailData.projectId, thick: radioButton.value, material: materialButton.value, relationType: PlateType.value })
    // 初始
    if (createNum.value === 1) {
      const tickData = []
      const materialData = []
      content.forEach(item => {
        tickData.push(item.thick)
        materialData.push(item.material)
        material.value = deDuPe(materialData).sort(sortArray)
        radioButtonData.value = deDuPe(tickData).sort(sortArray)
        console.log(' material.value', material.value, ' radioButtonData.value', radioButtonData.value)
      })
      createNum.value++
    }
    taskPackage.value = content
    ElMessage({ message: '投料成功！！！', type: 'success' })
  } catch (err) {
    console.log(err)
  }
  loadingPackage.value = false
}

function details(row) {
  console.log(row)
  innerVisible.value = true
  getByCutTaskId(row.areaId)
}

function resetQuery() {
  monomerValue.value = undefined
  areaValue.value = undefined
  CutPart(props.detailData.projectId)
}

function monomerChange() {
  CutPart(props.detailData.projectId, monomerValue.value)
  getArea(props.detailData.projectId, monomerValue.value)
}

function areaChange() {
  CutPart(props.detailData.projectId, monomerValue.value, areaValue.value)
}

function unique(arr) {
  return Array.from(new Set(arr))
}

async function nestWorkListClick() {
  try {
    workSelectList.value = unique(workSelectList.value.map((item) => { return item.id }))
    const message = await createOrder(workSelectList.value, { projectId: props.detailData.projectId })
    ElMessage({ message: message, type: 'success' })
  } catch (err) {
    console.log(err)
  }
}

function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (column.property === 'projectName' || column.property === 'monomerName') {
    if (rowIndex % 2 === 0) {
      return {
        rowspan: 2,
        colspan: 1
      }
    } else {
      return {
        rowspan: 0,
        colspan: 0
      }
    }
  }
}

function deDuPe(array) {
  return Array.from(new Set(array))
}

function sortArray(n1, n2) {
  return n1 - n2
}

function Query() {
  PlateType.value = undefined
  radioButton.value = undefined
  materialButton.value = undefined
  createTask()
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
.item-name {
  padding: 8px 16px;
  background-color: #ecf8ff;
  border-radius: 4px;
  border-left: 5px solid rgb(81, 113, 131);
  margin: 10px 0;
  margin-left: 5px;
  width: 150px;
}
.TaskPackage {
  margin-top: 30px;
}
.title-style {
  font-weight: 700;
  font-size: 18px;
  color: #000;
}
.enumBtn {
  font-size: 1px !important;
}
</style>

