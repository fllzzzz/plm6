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
      row-key="cutMachine.id"
      @expand-change="expandChange"
       returnSourceData
    >

       <el-table-column type="expand">
        <template v-slot='scope'>
        <common-table
              ref="tableRef"
              v-loading="scope.row.loadingList"
              :data="scope.row.subList"
              :max-height="maxHeight"
              row-key="id"
              style="width: 100%"
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
              min-width="80"> 
                <template v-slot="scope">
                    {{scope.row.cutInstructionId}} 
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
              min-width="60"> 
                <template v-slot="scope">
                    {{scope.row.thick}} 
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
              min-width="60"> 
                <template v-slot="scope">
                    {{scope.row.material}} 
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
              min-width="60"> 
                <template v-slot="scope">
                    {{scope.row.width}} * {{scope.row.length}}
                </template> 
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
              min-width="60"> 
                <!-- <template v-slot="scope">
                    {{scope.row.num}} 
                </template>  -->
                <span>1</span>
             </el-table-column> 
            <el-table-column
        v-if="columns.visible('plateState')"
        align="center"
        key="plateState"
        prop="plateState"
        label="完成状态"
        min-width="100px"
      >
        <template v-slot="scope">
          {{steelPlateEnum.VL[scope.row.plateState]}}
        </template>
      </el-table-column>

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
          <span>{{scope.row.projectNumber}}-{{ scope.row.projectName }}</span>
        </template>
      </el-table-column>
          <el-table-column align="center" label="查看套料成果" min-width="40" v-if="checkPermission(permission.detailResult)">
        <template v-slot="scope">
          <!-- <common-button
            v-if="checkPermission(permission.detail)"
            icon="el-icon-view"
            type="primary"
            size="mini"
            @click="showDetail(scope.row)"
          /> -->
          <common-button type="success" size="mini"  @click="nestResults(scope.row)">查看</common-button>
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
        label="产线名称"
        header-align="center"
        min-width="130px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.cutMachine.factory }}>{{scope.row.cutMachine.workshopInf}}>{{scope.row.cutMachine.productionLine}}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column v-if="columns.visible('plateNum')" align="center" key="plateNum" prop="plateNum" label="钢板总数" min-width="100px">
        <template v-slot="scope">
          <span>{{ scope.row.plateNum }}</span>
        </template>
      </el-table-column>  -->
            <!-- <el-table-column
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
      </el-table-column> -->
      <el-table-column v-if="columns.visible('plateNum')" align="center" key="plateNum" prop="plateNum" label="任务张数" min-width="100px">
        <template v-slot="scope">
          <span>{{ scope.row.plateNum }}</span>
        </template>
      </el-table-column> 
      <!-- <el-table-column v-if="columns.visible('partNum')" align="center" key="partNum" prop="partNum" label="零件数" min-width="100px">
        <template v-slot="scope">
          <span>{{ scope.row.partNum }}</span>
        </template>
      </el-table-column> -->
        <el-table-column v-if="columns.visible('plateWeight')" align="center" key="plateWeight" prop="plateWeight" label="任务重" min-width="100px">
        <template v-slot="scope">
          <span>{{ scope.row.plateWeight }}</span>
        </template>
      </el-table-column> 
              <!-- <el-table-column
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
      </el-table-column> -->
        <el-table-column v-if="columns.visible('finishNum')" align="center" key="finishNum" prop="finishNum" label="完成张数" min-width="100px">
        <template v-slot="scope">
          <span>{{ scope.row.finishNum }}</span>
        </template>
      </el-table-column> 
        <el-table-column v-if="columns.visible('finishWeight')" align="center" key="finishWeight" prop="finishWeight" label="完成重" min-width="100px">
        <template v-slot="scope">
          <span>{{ scope.row.finishWeight }}</span>
        </template>
      </el-table-column> 
      <!-- <el-table-column
       
        align="center"
        label="已切割"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.distributionNum }}</span>
        </template>
      </el-table-column> -->
      <!-- <el-table-column
      
        align="center"
        label="未切割"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.plateNum-scope.row.distributionNum}}</span>
        </template>
      </el-table-column> -->
       <!-- <el-table-column v-if="checkPermission(permission.download)"  align="center" label="套料成果" min-width="80px">
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
          <el-table-column v-if="checkPermission(permission.detail)" align="center" label="查看切割状态" min-width="80px">
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="primary" size="mini" @click="showDetail(scope.row)" />
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
    <task-schedul :detail-data="detailObj" v-model:visible="quicklyAssignVisible"></task-schedul>
     <detail-result @change="handleChange" :detail-data="detailObj" v-model:visible="specsVisible" />
  </div>
</template>

<script setup>
import { ref } from 'vue'
// import crudApi from '@/api/cutting/nestList'
import crudApi from '@/api/cutting/scheduling'
import crudApi1 from '@/api/cutting/project-data'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import detail from '@/views/cutting/cutting-work-order/device-task/template/steel-plate-list.vue'
// import taskSchedul from './module/task-scheduling'
import detailResult from '@/views/cutting/template/detail.vue'
import {steelPlateEnum} from '@enum-ms/cutting'
import useMaxHeight from '@compos/use-max-height'
import checkPermission from '@/utils/system/check-permission'
import { ElNotification } from 'element-plus'
// import { nestWorkListPM as permission } from '@/page-permission/cutting'
import { deviceTaskingPM as permission } from '@/page-permission/cutting'

const tableRef = ref()
const innerVisible = ref(false)
const specsVisible = ref(false)
const detailObj = ref()

// 钢板清单
const dataList = ref([])
const loadingList = ref(false)
const currentRow = ref()

// 暂停切割
const switchLoading = ref(false)

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud,  CRUD,columns } = useCRUD(
  {
    title: '设备任务',
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
  console.log(row, expandedRowsOrExpanded)
  row.loadingList = true
  try {
    const data = await crudApi1.get({
      nestingState:row.nestingState,
      sort: ['id.desc'],
      projectId:row.projectId,
      mac:row.cutMachine.mac
    })
    console.log('data', data)
    row.subList = data.content  

  } catch (error) {
    console.log('请求接口数据失败')
  }
  row.loadingList = false
}

const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractRecord',
  paginate: true,
  extraHeight: 40
})

const quicklyAssignVisible = ref(false) // 快速分配dlg

async function showDetail(row) {
  detailObj.value = row
  innerVisible.value = true
}

function download(row) {
  if (row.reportUrl !== null) {
    window.location.href = row.reportUrl
  } else {
    ElNotification({ title: '下载失败', message: '暂无套料成果 ', type: 'error' })
  }
}
//查看套料成果
function nestResults(row) {
  detailObj.value = row
  specsVisible.value = true
}

function taskScheduling(row) {
  detailObj.value = row
  quicklyAssignVisible.value = true
}

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