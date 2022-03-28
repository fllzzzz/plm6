<template>
  <common-drawer
    ref="drawerRef"
    title="任务排产"
    v-model="drawerVisible"
    direction="btt"
    :before-close="handleClose"
    :wrapper-closable="false"
    size="95%"
  >
    <template #titleRight>
      <common-button :disabled="!saveAble" type="primary" size="mini" @click="submit">预览</common-button>
    </template>
    <template #content>
      <div class="el-drawer-container">
        <div class="table-content">
          <common-table
            v-loading="tabLoading"
            :max-height="730"
            ref="tableRef"
            :data="plateData"
            row-key="id"
            empty-text="暂无数据"
            @selection-change="handleSelectionChange"
          >
            <el-table-column fixed type="selection" width="55" align="center" />
            <el-table-column fixed label="序号" type="index" align="center" width="60" />
            <el-table-column key="cutInstructionId" prop="cutInstructionId" :show-overflow-tooltip="true" label="指令号" min-width="100">
              <template v-slot="scope">
                <span>{{ scope.row.cutInstructionId }}</span>
              </template>
            </el-table-column>
            <el-table-column align="center" key="num" prop="num" :show-overflow-tooltip="true" label="数量" min-width="30">
              <template v-slot="scope">
                <span>{{ scope.row.num }}</span>
              </template>
            </el-table-column>
            <el-table-column key="material" align="center" prop="material" :show-overflow-tooltip="true" label="材质" min-width="40">
              <template v-slot="scope">
                <span>{{ scope.row.material }}</span>
              </template>
            </el-table-column>
            <el-table-column key="thick" prop="thick" :show-overflow-tooltip="true" label="厚度（mm）" min-width="40">
              <template v-slot="scope">
                <span>{{ scope.row.thick }}</span>
              </template>
            </el-table-column>
            <el-table-column key="width" prop="width" :show-overflow-tooltip="true" label="宽度（mm）" min-width="40">
              <template v-slot="scope">
                <span>{{ scope.row.width }}</span>
              </template>
            </el-table-column>
            <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="长度（mm）" min-width="40">
              <template v-slot="scope">
                <span>{{ scope.row.length }}</span>
              </template>
            </el-table-column>

            <el-table-column key="weight" prop="weight" :show-overflow-tooltip="true" label="单重（kg）" min-width="40">
              <template v-slot="scope">
                <span>{{ scope.row.weight }}</span>
              </template>
            </el-table-column>
            <el-table-column key="weight" prop="weight" :show-overflow-tooltip="true" label="总重（kg）" min-width="40">
              <template v-slot="scope">
                <span>{{ scope.row.weight }}</span>
              </template>
            </el-table-column>
          </common-table>
          <el-pagination
            v-model:page-size="page.size"
            v-model:current-page="page.page"
            :total="page.total"
            style="margin-top: 8px"
            layout="total, prev, pager, next, sizes"
            @size-change="sizeChangeHandler($event)"
            @current-change="pageChangeHandler"
          />
        </div>
        <div class="line-content" :style="{ 'max-height': `${maxHeight}px` }">
          <div class="tip">
            <span>* 注意：</span>
            <div>
              <span style="display: block">1. 构件未被禁止操作，构件任务未暂停，构件未处于报废或二次利用状态。</span>
              <span style="display: block">2. 只能选择一条生产线，所勾选构件的未分配数量会全部分配给该生产线。</span>
            </div>
          </div>
          <machine :machineData="machineData" :selectLineId="selectLineId" @change="handleChange" isSingle />
        </div>
      </div>
    </template>
  </common-drawer>
  <preview
    @closeHook="closeHook"
    @taskIssue="taskIssue"
    :detail-data="multipleSelection"
    :select-line="selectLine"
    v-model:visible="specsVisible"
  />
</template>

<script setup>
import { computed, defineProps, defineEmits, ref } from 'vue'
import { get, getMachine } from '@/api/cutting/project-data'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import { assign } from '@/api/cutting/machine'
import { ElMessage } from 'element-plus'
import machine from '../machie/index'
import preview from '../preview/index'

const emit = defineEmits(['update:visible', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  data: {
    type: Array,
    default: () => []
  },
  detailData: {
    type: Object,
    required: true
  }
})

const tableRef = ref()
const drawerRef = ref()
const selectLine = ref()
const selectLineId = ref()
const plateData = ref([]) // 可分配列表
const machineData = ref([])
const multipleSelection = ref([])
const specsVisible = ref(false)

const idList = ref([])

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', closeHook, showHook })

// 高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.common-drawer',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body', '.el-drawer-container'],
    navbar: false,
    extraHeight: 20,
    clientHRepMainH: true
  },
  drawerRef
)

const page = {
  size: 20,
  page: 1,
  total: null
}

const tabLoading = ref(false)
const saveAble = computed(() => multipleSelection.value && multipleSelection.value.length > 0 && selectLineId.value)

function showHook() {
  if (props.detailData) {
    plateDataGet()
    getReport()
  }
}
function closeHook() {
  page.size = 20
  page.page = 1
}

async function plateDataGet() {
  tabLoading.value = true
  try {
    const data = await get({ projectId: props.detailData.projectId, pageSize: page.size, pageNumber: page.page })
    page.total = data.totalElements
    plateData.value = data.content.filter(item => !(item.mac))
  } catch (err) {
    console.log('钢板清单页面接口报错', err)
  }
  tabLoading.value = false
}
async function getReport() {
  const { content } = await getMachine()
  machineData.value = content
}
function submit() {
  specsVisible.value = true
}

function handleChange(row) {
  selectLine.value = row
  selectLineId.value = row.id
}

function handleSelectionChange(val) {
  multipleSelection.value = val
}

async function taskIssue() {
  try {
    idList.value = multipleSelection.value.map((item) => { return item.id })
    const message = await assign({ mac: selectLine.value.mac }, idList.value)
    ElMessage({ message: message, type: 'success' })
    specsVisible.value = false

    plateDataGet()
  } catch (err) {
    console.log(err)
  }
}

function sizeChangeHandler(e) {
  page.size = e
  plateDataGet()
}

function pageChangeHandler(a) {
  page.page = a
  plateDataGet()
}
</script>

<style lang="scss" scoped>
.el-drawer-container {
  box-sizing: border-box;
  display: flex;
  flex-direction: row;
  justify-content: center;
  align-items: flex-start;
}

.el-drawer-container .table-content {
  flex-grow: 1;
  width: 70%;
}

.line-content {
  padding: 14px 20px 20px 40px;
  box-sizing: border-box;
  overflow: auto;
  flex-grow: 1;
  .tip {
    display: flex;
    flex-direction: row;
    justify-content: flex-start;
    align-items: flex-start;
    color: red;
    font-size: 13px;
    margin-bottom: 15px;
    line-height: 20px;

    > span {
      display: inline-block;
    }

    > span:nth-child(1) {
      width: 50px;
      flex-shrink: 0;
    }
  }
}
</style>
