<template>
  <div class="app-container">
    <!-- 工具栏 -->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight - 50"
      style="width: 100%"
      row-key="projectId"
      @expand-change="expandChange"
      :expand-row-keys="expandArr"
      returnSourceData
    >
      <el-table-column type="expand">
        <template v-slot="scope">
          <common-table
            ref="tableRef"
            v-loading="scope.row.loadingList"
            :data="scope.row.subList"
            row-key="id"
            :max-height="maxHeight"
            style="width: 1000%"
            border
          >
            <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
            <el-table-column
              v-if="columns.visible('cutInstructionId')"
              header-align="center"
              key="cutInstructionId"
              prop="cutInstructionId"
              :show-overflow-tooltip="true"
              style="width: 100%"
              label="切割指令号"
              align="center"
              min-width="60"
            >
              <template v-slot="scope">
                {{ scope.row.cutInstructionId }}
              </template>
            </el-table-column>
            <el-table-column
              v-if="columns.visible('thick')"
              header-align="center"
              key="thick"
              prop="thick"
              :show-overflow-tooltip="true"
              style="width: 100%"
              label="钢板厚度"
              align="center"
              min-width="60"
            >
              <template v-slot="scope">
                {{ scope.row.thick }}
              </template>
            </el-table-column>
            <el-table-column
              v-if="columns.visible('material')"
              header-align="center"
              key="material"
              prop="material"
              :show-overflow-tooltip="true"
              style="width: 100%"
              label="材质"
              align="center"
              min-width="60"
            >
              <template v-slot="scope">
                {{ scope.row.material }}
              </template>
            </el-table-column>
            <el-table-column
              v-if="columns.visible('specification')"
              header-align="center"
              key="specification"
              prop="specification"
              :show-overflow-tooltip="true"
              style="width: 100%"
              label="规格"
              align="center"
              min-width="60"
            >
              <template v-slot="scope"> {{ scope.row.width }} * {{ scope.row.length }} </template>
            </el-table-column>
            <el-table-column
              v-if="columns.visible('num')"
              header-align="center"
              key="num"
              prop="num"
              :show-overflow-tooltip="true"
              style="width: 100%"
              label="数量"
              align="center"
              min-width="60"
            >
              <span>1</span>
            </el-table-column>
            <el-table-column
              v-if="columns.visible('plateState')"
              align="center"
              key="plateState"
              prop="plateState"
              label="状态"
              min-width="60px"
            >
              <template v-slot="scope">
                <span>{{ steelPlateEnum.VL[scope.row.plateState] }}</span>
              </template>
            </el-table-column>
            <el-table-column align="center" :show-overflow-tooltip="true" label="套料成果" min-width="70">
              <template v-slot="scope">
                <common-button type="success" size="mini" @click="nestResults(scope.row)">查看</common-button>
              </template>
            </el-table-column>
            <el-table-column
              align="center"
              v-if="columns.visible('machineType')"
              key="machineType"
              prop="machineType"
              :show-overflow-tooltip="true"
              label="切割方式"
              min-width="70"
            >
              <template v-slot="scope">
                <span v-if="scope.row.mac && scope.row.machineType === machineTypeEnum.FLAME_CUTTING.V">火焰切割设备</span>
                <span v-if="scope.row.mac && scope.row.machineType === machineTypeEnum.PLASMA_CUTTING.V">等离子切割设备</span>
                <span v-if="scope.row.mac && scope.row.machineType === machineTypeEnum.LASER_CUTTING.V">激光切割设备</span>
                <!-- {{scope.row.machineType}} -->
              </template>
            </el-table-column>
            <el-table-column fixed="right" align="center" label="排产操作" min-width="90px">
              <template v-slot="scope">
                <common-button
                  v-if="scope.row.plateState === '0'&&checkPermission(permission.Production)"
                  @click.stop="taskScheduling(scope.row)"
                  type="success"
                  size="mini"
                  >任务排产</common-button
                >
                <!-- <common-button v-if="scope.row.plateState === '1'" @click="taskIssuance(scope.row)" type="warning" size="mini">任务下发</common-button> -->
                <!-- <common-button  @click="taskIssuance(scope.row)" type="warning" size="mini">任务下发</common-button> -->
                <el-popover
                  v-if="scope.row.plateState === '1'"
                  v-model:visible="scope.row.taskLoading"
                  placement="top"
                  width="180"
                  trigger="manual"
                  @show="onPopoverTaskingShow"
                  @hide="onPopoverTaskingHide"
                >
                  <p>选择任务下发操作，确认任务下发？</p>
                  <div style="text-align: right; margin: 0">
                    <common-button size="mini" type="text" @click="cancelTask(scope.row)">取消</common-button>
                    <common-button type="primary" size="mini" @click="taskClick(scope.row)">确定</common-button>
                  </div>
                  <template #reference>
                    <common-button v-if="checkPermission(permission.taskLoading)" type="warning" size="mini" @click.stop="taskIssuance(scope.row)"> 任务下发 </common-button>
                  </template>
                </el-popover>
                <!-- <common-button v-if="scope.row.plateState === '1'" @click.stop="clean(scope.row)" type="danger" size="mini"
                  >清除任务</common-button
                > -->
                <el-popover
                  v-if="scope.row.plateState === '1'"
                  v-model:visible="scope.row.cleanLoading"
                  placement="top"
                  width="180"
                  trigger="manual"
                  @show="onPopoverCleaningShow"
                  @hide="onPopoverCleaningHide"
                >
                  <p>选择清除任务操作，确认清除任务？</p>
                  <div style="text-align: right; margin: 0">
                    <common-button size="mini" type="text" @click="cancelClean(scope.row)">取消</common-button>
                    <common-button type="primary" size="mini" @click="taskClean(scope.row)">确定</common-button>
                  </div>
                  <template #reference>
                     <common-button
                      v-if="checkPermission(permission.cleanLoading)"
                      @click.stop="clean(scope.row)"
                      type="danger"
                      size="mini"
                  >任务清除</common-button
                >
                  </template>
                </el-popover>

                <!-- <common-button v-if="scope.row.plateState === '2'" @click.stop="resetTasking(scope.row)" type="primary" size="mini"
                  >重置任务</common-button
                > -->
                <el-popover
                  v-if="scope.row.plateState === '2'"
                  v-model:visible="scope.row.resetLoading"
                  placement="top"
                  width="180"
                  trigger="manual"
                  @show="onPopoverResetShow"
                  @hide="onPopoverResetHide"
                >
                  <p>选择重置任务操作，确认重置任务？</p>
                  <div style="text-align: right; margin: 0">
                    <common-button size="mini" type="text" @click="cancelReset(scope.row)">取消</common-button>
                    <common-button type="primary" size="mini" @click="taskReset(scope.row)">确定</common-button>
                  </div>
                  <template #reference>
                        <common-button v-if="checkPermission(permission.resetLoading)" @click.stop="resetTasking(scope.row)" type="primary" size="mini">任务重置</common-button>
                  </template>
                </el-popover>
              </template>
            </el-table-column>
            <!-- <el-table-column
              v-if="columns.visible('reduce')"
              header-align="center"
              key="reduce"
              prop="reduce"
              :show-overflow-tooltip="true"
              style="width: 100%"
              label="零件重量"
              align="center"
              min-width="60">
                <template v-slot="scope">
                    {{scope.row.reduce}}
                </template>
             </el-table-column>  -->
            <!-- <el-table-column
              v-if="columns.visible('relationType')"
              header-align="center"
              key="relationType"
              prop="relationType"
              :show-overflow-tooltip="true"
              style="width: 100%"
              label="零件属性"
              align="center"
              min-width="60">
              <template v-slot="scope"> -->
            <!-- <el-tag type="success">
                  {{PlateTypeEnum.VL[scope.row.relationType]}}
                </el-tag> -->
            <!-- <el-tag  v-if='scope.row.relationType&&scope.row.relationType===2' type="success">
                  零件板
                </el-tag>
                <el-tag  v-else-if='scope.row.relationType&&scope.row.relationType===16' type="danger">
                  翼腹板
                </el-tag>

              </template>
             </el-table-column>  -->
            <!-- <el-table-column
        v-if="columns.visible('state')"
        align="center"
        key="state"
        prop="state"
        :show-overflow-tooltip="true"
        label="状态"
        min-width="40"
      >
        <template v-slot="scope">
          <span>
            <el-tag style="width: 100%" effect="plain" v-if="scope.row.state && scope.row.state === '1'" type="warning">
              部分套料
            </el-tag>
            <el-tag style="width: 100%" effect="plain" v-else-if="scope.row.state && scope.row.state === '2'" type="success">
              套料结束
            </el-tag>
            <el-tag style="width: 100%" effect="plain" v-else-if="scope.row.state && scope.row.state === '0'" type="danger">
              未套料
            </el-tag>
          </span>
        </template>
      </el-table-column> -->
            <!-- <el-table-column
        v-if="columns.visible('currentName')"
        align="center"
        key="currentName"
        prop="currentName"
        :show-overflow-tooltip="true"
        label="投料人"
        min-width="40"
      >
        <template v-slot="scope">
          <span>{{ scope.row.currentName }} {{scope.row.importTime}}</span>
        </template>
      </el-table-column> -->
            <!-- <el-table-column width="250px" :show-overflow-tooltip="true" label="操作" align="center"> -->
            <!-- <template v-slot="scope"> -->
            <!-- <el-popover
            v-if="scope.row.nestingState === 1"
            v-model:visible="scope.row.deleteBtn"
            placement="top"
            width="180"
            trigger="click"
            @show="onPopoverDelClickShow"
            @hide="onPopoverDelClickHide"
          >
            <p>选择删除操作，让数据删除？</p>
            <div style="text-align: right; margin: 0">
              <common-button size="mini" type="text" @click="cancelDeleteBtn(scope.row)">取消</common-button>
              <common-button type="primary" size="mini" @click="delClick(scope.row)">确定</common-button>
            </div>
            <template #reference>
              <common-button type="danger" icon="el-icon-delete" size="mini" @click.stop="toDeleteBtn(scope.row)" />
            </template>
          </el-popover> -->
            <!-- </template> -->
            <!-- </el-table-column> -->
          </common-table>
        </template>
      </el-table-column>
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('projectName')"
        key="projectName"
        prop="projectName"
        :show-overflow-tooltip="true"
        label="所属项目"
        header-align="center"
        min-width="130px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.projectNumber }}-{{ scope.row.projectName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('plateNum')" align="center" key="plateNum" prop="plateNum" label="钢板总数" min-width="100px">
        <template v-slot="scope">
          <span>{{ scope.row.plateNum }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('plateWeight')"
        align="center"
        key="plateWeight"
        prop="plateWeight"
        label="钢板总重"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.plateWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('partNum')" align="center" key="partNum" prop="partNum" label="零件数" min-width="100px">
        <template v-slot="scope">
          <span>{{ scope.row.partNum }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('partWeight')"
        align="center"
        key="partWeight"
        prop="partWeight"
        label="零件重"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.partWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('distributionNum')"
        align="center"
        key="distributionNum"
        prop="distributionNum"
        label="已排产"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.distributionNum }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('distribution')"
        align="center"
        key="distribution"
        prop="distribution"
        label="未排产"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.plateNum - scope.row.distributionNum }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="checkPermission(permission.download)" align="center" label="套料成果" min-width="80px">
        <template v-slot="scope">
          <common-button
            :disabled="scope.row.reportUrl === null"
            icon="el-icon-download"
            @click="download(scope.row)"
            type="warning"
            size="mini"
          />
        </template>
      </el-table-column>
       <el-table-column
        v-if="columns.visible('nestingState')"
        align="center"
        key="nestingState"
        prop="nestingState"
        :show-overflow-tooltip="true"
        label="状态"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>
            <el-tag style="width: 100%" effect="plain"  v-if="scope.row.nestingState === schedulingEnum.NOT_Scheduling_up.V" type="danger">
              未排产
            </el-tag>
            <el-tag style="width: 100%" effect="plain" v-if="scope.row.nestingState && scope.row.nestingState === schedulingEnum.PARTIAL_Scheduling_up.V" type="warning">
              部分排产
            </el-tag>
            <el-tag style="width: 100%" effect="plain" v-if="scope.row.nestingState && scope.row.nestingState === schedulingEnum.Scheduling_up.V" type="success">
              排产结束
            </el-tag>
          </span>
        </template>
      </el-table-column>
      <!-- <el-table-column
        v-if="columns.visible('distributionNum')"
        align="center"
        key="distributionNum"
        prop="distributionNum"
        label="排产数"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.distributionNum }}</span>
        </template>
      </el-table-column> -->

      <!-- <el-table-column
        v-if="columns.visible('distributionWeight')"
        align="center"
        key="distributionWeight"
        prop="distributionWeight"
        label="排产量"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.distributionWeight }}</span>
        </template>
      </el-table-column> -->

      <!-- <el-table-column
        v-if="columns.visible('distributionPartNum')"
        align="center"
        key="distributionPartNum"
        prop="distributionPartNum"
        label="已排零件数"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.distributionPartNum }}</span>
        </template>
      </el-table-column> -->

      <!-- <el-table-column
        v-if="columns.visible('distributionPartWeight')"
        align="center"
        key="distributionPartWeight"
        prop="distributionPartWeight"
        label="已排零件重"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.distributionPartWeight }}</span>
        </template>
      </el-table-column> -->

      <!-- <el-table-column v-if="checkPermission(permission.detail)" fixed="right" align="center" label="钢板清单" min-width="80px">
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="primary" size="mini" @click="showDetail(scope.row)" />
        </template>
      </el-table-column>
      <el-table-column v-if="checkPermission(permission.Production)" fixed="right" align="center" label="操作" min-width="90px">
        <template v-slot="scope">
          <common-button @click="taskScheduling(scope.row)" type="success" size="mini">任务排产</common-button>
        </template>
      </el-table-column> -->

      <!-- <el-table-column v-if="checkPermission(permission.download)" fixed="right" align="center" label="套料成果" min-width="80px">
        <template v-slot="scope">
          <common-button
            :disabled="scope.row.reportUrl === null"
            icon="el-icon-download"
            @click="download(scope.row)"
            type="warning"
            size="mini"
          />
        </template>
      </el-table-column> -->
      <!-- <el-table-column align="center" :show-overflow-tooltip="true" label="钢板量（张 | kg）" min-width="80">
        <template v-slot="scope">
          <span class="quantity-mete-show tc-primary">
            <span class="left">{{ scope.row.plateNum }}</span>
            <span class="line">|</span>
            <span class="right">{{ scope.row.plateWeight }}</span>
          </span>
        </template>
      </el-table-column>
      <el-table-column align="center" :show-overflow-tooltip="true" label="排产数" min-width="80">
        <template v-slot="scope">
          <span class="quantity-mete-show tc-success">
            <span class="left">{{ scope.row.distributionNum }}</span>
            <span class="line">|</span>
            <span class="right">{{ scope.row.distributionWeight }}</span>
          </span>
        </template>
      </el-table-column>
      <el-table-column align="center" :show-overflow-tooltip="true" label="零件数" min-width="80">
        <template v-slot="scope">
          <span class="quantity-mete-show tc-primary">
            <span class="left">{{ scope.row.partNum }}</span>
            <span class="line">|</span>
            <span class="right">{{ scope.row.partWeight }}</span>
          </span>
        </template>
      </el-table-column>
      <el-table-column align="center" :show-overflow-tooltip="true" label="已排零件数" min-width="80">
        <template v-slot="scope">
          <span class="quantity-mete-show tc-success">
            <span class="left">{{ scope.row.distributionPartNum }}</span>
            <span class="line">|</span>
            <span class="right">{{ scope.row.distributionPartWeight }}</span>
          </span>
        </template>
      </el-table-column> -->
    </common-table>
    <!--分页组件-->
    <pagination />

    <!-- 钢板清单 -->
    <detail :detail-data="detailObj" v-model:visible="innerVisible" />
    <!-- <task-schedul :detail-data="detailObj" v-model:visible="quicklyAssignVisible" @success="crud.toQuery"></task-schedul> -->
    <task-schedul :detail-data="detailObj" v-model:visible="quicklyAssignVisible" @success="plateDataGet"></task-schedul>
    <detail-result @change="handleChange" :detail-data="detailObj" v-model:visible="specsVisible" />
  </div>
</template>

<script setup>
import crudApi from '@/api/cutting/nestList'
import crudApi1 from '@/api/cutting/project-data'
import { getMac, sentTask, cleanTask, resetTask } from '@/api/cutting/machine'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import detail from '@/views/cutting/template/steel-plate-list.vue'
import taskSchedul from './module/task-scheduling'
import useMaxHeight from '@compos/use-max-height'
import { steelPlateEnum, schedulingEnum, machineTypeEnum } from '@enum-ms/cutting'
import checkPermission from '@/utils/system/check-permission'
import { ElNotification, ElMessage } from 'element-plus'
// import { nestWorkListPM as permission } from '@/page-permission/cutting'
import { cuttingWorkingPM as permission } from '@/page-permission/cutting'
import useVisible from '@compos/use-visible'
import detailResult from '@/views/cutting/template/detail.vue'
import { defineProps, defineEmits, ref } from 'vue'

const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
  detailData: {
    type: Object
  }
})

// const permission = inject('permission')
const emit = defineEmits(['update:visible'])
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', closeHook, showHook })

function showHook() {
  if (props.detailData) {
    plateDataGet()
  }
}

function closeHook() {
  emit('colesHook')
  plateDataGet()
}

const tableRef = ref()
const innerVisible = ref(false)
const expandArr = ref([])
// const detailObj = ref()
// 钢板清单
// const dataList = ref([])
// const loadingList = ref(false)
// const tabLoading = ref(false)
// const plateData = ref([])
// 套料成果
const selectLineId = ref()
const detailObj = ref([])
const specsVisible = ref(false)
// 任务下发
const taskLoading = ref(false)
// 清除任务
const cleanLoading = ref(false)
// 重置任务
const resetLoading = ref(false)
// 当前行
const currentRow = ref({})

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud, CRUD, columns } = useCRUD(
  {
    title: '切割工单',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)
CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v, index) => {
    v.subList = []
    v.rowIndex = index
    return v
  })
}

// 请求接口数据
async function expandChange(row, expandedRowsOrExpanded) {
  if (!checkPermission(permission.detail)) {
    return false
  }
  console.log(row, expandedRowsOrExpanded, 'expandChange')
  currentRow.value = row
  row.loadingList = true
  try {
    const data = await crudApi1.get({
      // nestingState:row.nestingState,
      projectId: currentRow.value.projectId,
      mac: currentRow.value.mac
    })
    // data.content.forEach((item,index) => {
    //     if(item.id === currentRow.id) {
    //         data.content[index].plateData =data.content
    //     }
    // });
    console.log('data', data)

    for (var i = 0; i < data.content.length; i++) {
      console.log(data.content[i].mac)
      if (data.content[i].mac !== null) {
        const macData = await getMac(data.content[i].mac)
        data.content[i].machineType = macData.machineType
      }
    }
    // data.content.map(async (item)=>{
    //   const macData = await getMac(item.mac)
    //   item.machineType = macData.machineType
    // })
    // dataList.value[row.index] = data.content
    // plateData.value = data.content
    row.subList = data.content
  } catch (error) {
    console.log('请求接口数据失败')
  }
  row.loadingList = false
}
// 请求钢板接口数据
async function plateDataGet() {
  // console.log(row);
  // currentRow.value = row
  try {
    // const content = await crudApi1.get(props.detailData)
    const { content } = await crudApi1.get({ projectId: currentRow.value.projectId, mac: currentRow.value.mac })
    console.log('content', content)

    for (var i = 0; i < content.length; i++) {
      console.log(content[i].mac)
      if (content[i].mac !== null) {
        const macData = await getMac(content[i].mac)
        content[i].machineType = macData.machineType
      }
    }
    // plateData.value = content
    crud.data[currentRow.value.rowIndex].subList = content
    // console.log(' plateData.value', plateData.value)
  } catch (err) {
    console.log('钢板清单页面接口报错', err)
  }
}

// 任务下发
async function taskClick(row) {
  try {
    const sentData = []
    sentData.push(row.id)
    const message = await sentTask(sentData)
    ElMessage({ message: message, type: 'success' })
    const index = expandArr.value.find(v => v.id === currentRow.value.projectId)
    expandArr.value.push(index)
    plateDataGet()
    crud.toQuery()
    // plateData.value = content
  } catch (error) {
    console.log('请求下发任务的接口失败')
  }
  row.taskLoading = false
}
// 隐藏任务下发的提示框
function handleDocumentTaskingClick(event) {
  taskLoading.value = false
}
// 隐藏任务清楚的提示框
function handleDocumentCleaningClick(event) {
  cleanLoading.value = false
}
// 隐藏任务重置的提示框
function handleDocumentResetClick(event) {
  resetLoading.value = false
}

// 打开下发任务提示窗
function onPopoverTaskingShow() {
  setTimeout(() => {
    document.addEventListener('click', handleDocumentTaskingClick, { passive: false })
  }, 0)
}

// 隐藏下发任务提示窗
function onPopoverTaskingHide() {
  document.removeEventListener('click', handleDocumentTaskingClick)
}
// 打开任务清除提示框
function onPopoverCleaningShow() {
  setTimeout(() => {
    document.addEventListener('click', handleDocumentCleaningClick, { passive: false })
  }, 0)
}
// 隐藏任务清除提示框
function onPopoverCleaningHide() {
  document.removeEventListener('click', handleDocumentCleaningClick)
}
// 打开任务重置提示框
function onPopoverResetShow() {
  setTimeout(() => {
    document.addEventListener('click', handleDocumentResetClick, { passive: false })
  }, 0)
}
// 隐藏任务重置提示框
function onPopoverResetHide() {
  document.removeEventListener('click', handleDocumentResetClick)
}

// 点击任务下发的按钮
function taskIssuance(row) {
  row.taskLoading = true
}
// 点击任务清除的按钮
function clean(row) {
  row.cleanLoading = true
}
// 点击任务重置的按钮
function resetTasking(row) {
  row.resetLoading = true
}
// 点击取消任务下发的按钮
function cancelTask(row) {
  row.taskLoading = false
}
// 点击取消任务清除的按钮
function cancelClean(row) {
  row.cleanLoading = false
}
// 点击取消任务重置的按钮
function cancelReset(row) {
  row.resetLoading = false
}

// 清除状态为已分配的任务
async function taskClean(row) {
  // currentRow.value = row
  try {
    const list = []
    list.push(row.id)
    const message = await cleanTask({ mac: row.mac }, list)
    ElMessage({ type: 'error', message: message })
    plateDataGet()

    // const data = await crudApi1.get({projectId:currentRow.value.projectId,mac:currentRow.value.mac})
    // const data = await crudApi1.get({projectId:currentRow.value.projectId})

    //  通过mac地址查机器设备
    // for(var i = 0; i < data.content.length; i++) {
    //     if(data.content[i].mac) {
    //         const macData = await getMac(data.content[i].mac)
    //         // if(macData !== null) {
    //           data.content[i].machineType = macData.machineType
    //           data.content[i].factory = macData.factory
    //           data.content[i].workshopInf = macData.workshopInf
    //           data.content[i].productionLine = macData.productionLine
    //           // }
    //     }
    // }
    //  plateData.value = data.content
  } catch (error) {
    console.log('请求清除任务的接口数据失败')
  }
  cleanLoading.value = false
}
// 重置任务
async function taskReset(row) {
  try {
    const resetData = []
    resetData.push(row.id)
    const message = await resetTask(resetData)
    ElMessage({ type: 'warning', message: message })
    const index = expandArr.value.find(v => v.id === currentRow.value.projectId)
    expandArr.value.push(index)
    plateDataGet()
    crud.toQuery()
  } catch (error) {
    console.log('请求重置任务的接口失败')
  }
  resetLoading.value = false
}

// 查看套料成果
function nestResults(row) {
  detailObj.value = row
  specsVisible.value = true
}

function handleChange(row) {
  selectLineId.value = row.id
}

const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractRecord',
  paginate: true,
  extraHeight: 40
})

const quicklyAssignVisible = ref(false) // 快速分配dlg

// async function showDetail(row) {
//   detailObj.value = row
//   innerVisible.value = true
// }

function download(row) {
  if (row.reportUrl !== null) {
    window.location.href = row.reportUrl
  } else {
    ElNotification({ title: '下载失败', message: '暂无套料成果 ', type: 'error' })
  }
}

function taskScheduling(row) {
  detailObj.value = row
  quicklyAssignVisible.value = true
  // emit('success')
}

// CRUD.HOOK.handleRefresh = (crud, data) => {
//   data.data.content = data.data.content.map((v, i) => {
//     v.index = i
//     return v
//   })
// }
</script>

<style lang="scss">
.quantity-mete-show {
  display: flex;

  .left {
    width: 50%;
    text-align: right;
  }

  .right {
    width: 50%;
    text-align: left;
  }

  .line {
    width: 15px;
    text-align: center;
  }
}
</style>
