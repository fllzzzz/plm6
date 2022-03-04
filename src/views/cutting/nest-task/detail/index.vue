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
      <common-button :disabled="disableTask" type="primary" size="mini" @click="createTask">创建任务包</common-button>
      <common-button size="mini" @click="handleClose">关 闭</common-button>
    </template>
    <div class="flex-rss">
      <common-table
        :max-height="200"
        ref="tableRef"
        v-loading="loadingPart"
        :data="partData"
        style="width: 100%"
        @selection-change="handleSelectionChange"
        row-key="id"
      >
        <!-- <el-table-column :selectable='checkAble' type="selection" align="center" width="55" /> -->
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
      </common-table>
    </div>
    <div class="TaskPackage">
      <common-table v-loading="loadingPackage" ref="tableRef" :data="taskPackage" :max-height="600" style="width: 100%" row-key="id">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column align="center" key="areaName" prop="areaName" :show-overflow-tooltip="true" label="所选单体" min-width="100px">
          <template v-slot="scope">
            <span>{{ scope.row.areaName }}</span>
          </template>
        </el-table-column>
        <el-table-column align="left" key="thick" prop="thick" :show-overflow-tooltip="true" label="厚度（mm）" min-width="40">
          <template v-slot="scope">
            <span>{{ scope.row.thick }}</span>
            <el-tag style="float: right; margin-right: 10px" v-if="scope.row.relationType && scope.row.relationType === 2" type="success">
              零件板
            </el-tag>
            <el-tag style="float: right; margin-right: 10px" v-if="scope.row.relationType && scope.row.relationType === 16" type="danger">
              翼腹板
            </el-tag>
          </template>
        </el-table-column>
        <el-table-column align="center" key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="40">
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column align="right" key="sum" prop="sum" :show-overflow-tooltip="true" label="零件数量（件）" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.sum }}</span>
          </template>
        </el-table-column>
        <el-table-column align="right" key="reduce" prop="reduce" :show-overflow-tooltip="true" label="零件重量（kg）" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.reduce }}</span>
          </template>
        </el-table-column>
        <el-table-column align="right" key="totalNum" prop="totalNum" :show-overflow-tooltip="true" label="累计下发数（件）" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.totalNum }}</span>
          </template>
        </el-table-column>
        <el-table-column
          align="right"
          key="totalWeight"
          prop="totalWeight"
          :show-overflow-tooltip="true"
          label="累计下发量（kg）"
          min-width="60"
        >
          <template v-slot="scope">
            <span>{{ scope.row.totalWeight }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" :show-overflow-tooltip="true" label="操  作" min-width="60">
          <template v-slot="scope">
            <common-button v-if="scope.row.state === '0'" @click="preservation(scope.row)" type="success" size="mini">去套料</common-button>
            <common-button v-else @click="details(scope.row)" type="primary" size="mini">查 看</common-button>
          </template>
        </el-table-column>
      </common-table>
    </div>
  </common-dialog>
  <common-dialog width="50%" title="零件清单" append-to-body v-model="innerVisible">
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
      <el-table-column key="netWeight" prop="netWeight" :show-overflow-tooltip="true" label="单重" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.netWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column key="totalNetWeight" prop="totalNetWeight" :show-overflow-tooltip="true" label="总重" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.totalNetWeight }}</span>
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { ElMessage } from 'element-plus'
import useVisible from '@compos/use-visible'
import { defineProps, defineEmits, ref } from 'vue'
import { getTaskByProjectId, creatTaskPack, getTaskPack, uploadTask, ByCutTaskId, area, monomer } from '@/api/cutting/radan-controller'
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
const areaIdList = ref([])
const taskPackage = ref([]) // 任务包数据
const PartByCutTaskIdData = ref([]) // 零件清单
const innerVisible = ref(false)
const disableTask = ref(true)
const titleName = ref('')
const loadingPart = ref(false)
const loadingPackage = ref(false)

const areaValue = ref()
const monomerValue = ref()

const areaList = ref([])// 区域
const monomerList = ref([])// 单体

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

async function getPack(params) {
  loadingPackage.value = true
  try {
    const { content } = await getTaskPack({ projectId: props.detailData.projectId })
    taskPackage.value = content
  } catch (err) {
    console.log(err)
  }
  loadingPackage.value = false
}

async function getByCutTaskId(params) {
  const { content } = await ByCutTaskId(params)
  PartByCutTaskIdData.value = content
}

async function TaskPack(obj, id, name) {
  const message = await creatTaskPack(obj, { projectId: id, cutTaskName: name })
  ElMessage({ message: message, type: 'success' })
  getPack()
}

function handleSelectionChange(val) {
  if (val.length > 0) {
    disableTask.value = false
    handleSelection.value = val
  } else {
    handleSelection.value = []
    disableTask.value = true
  }
}

function showHook() {
  if (props.detailData) {
    CutPart() // 1
    getPack() // 2
    getMonomer()// 单体
    // getArea() // 单元
  }
}

function closeHook() {
  disableTask.value = true
  taskPackage.value = []
  monomerValue.value = undefined
}

async function getMonomer() {
  try {
    const { content } = await monomer({ projectId: props.detailData.projectId, productType: 1 })
    monomerList.value = content
    console.log('monomerList.value', monomerList.value)
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

async function getUploadTask(row) {
  const message = await uploadTask(row)
  ElMessage({ message: message, type: 'success' })
  getPack()
}

function createTask() {
  try {
    areaIdList.value = []
    areaIdList.value = handleSelection.value.map((item) => {
      return item.areaId
    })
    areaIdList.value = unique(areaIdList.value)
    TaskPack(areaIdList.value, props.detailData.projectId, 'text')
  } catch (err) {
    console.log(err)
  }
}
function preservation(row) {
  getUploadTask(row)
  getPack()
}
function details(row) {
  innerVisible.value = true
  getByCutTaskId({ cutTaskId: row.cutTaskId })
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
</style>

