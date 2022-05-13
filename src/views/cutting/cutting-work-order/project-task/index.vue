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
       returnSourceData
    >

       <el-table-column type="expand">
        <template v-slot='scope'>
        <common-table
              ref="tableRef"
              v-loading="scope.row.loadingList"
              :data="scope.row.subList"
              @change="handleChange"
              isSingle
              row-key="id"
              :max-height="maxHeight"
              :showEmptySymbol="false"
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
              min-width="100"> 
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
              min-width="60"
              > 
                  
                  <!-- <el-tooltip
                    class="box-item"
                    effect="dark"
                    content="点击钢板的厚度查看套料文件"
                    placement="right-start"
                  >
                    <el-button @click="nestResults(scope.row)">{{scope.row.thick}}</el-button>
                  </el-tooltip>            -->
                <template v-slot:header>
                  <el-tooltip class="item" effect="dark" :content="`点击钢板的厚度查看套料文件`" placement="right-start">
                    <div style="display: inline-block">
                      <span style="margin-right:3px">钢板厚度</span>
                      <i class="el-icon-info" />
                    </div>
                  </el-tooltip>
                </template> 
                <template v-slot="scope">  
                    <el-button v-if="checkPermission(permission.detailResult)" @click="nestResults(scope.row)">{{scope.row.thick}}</el-button>
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
        key="plateState"
        prop="plateState"
        align="center"
        label="完成状态"
        min-width="60px"
      >
        <template v-slot="scope">
          <span>{{steelPlateEnum.VL[scope.row.plateState]}}</span>
        </template>
      </el-table-column>

        <el-table-column align="center" 
          v-if="columns.visible('machineType')"
        key="machineType"
        prop="machineType"
        :show-overflow-tooltip="true" label="切割方式" min-width="70">
          <template v-slot="scope">
            <span v-if="scope.row.mac&&scope.row.machineType === '0'">火焰切割设备</span>
            <span v-if="scope.row.mac&&scope.row.machineType === '1'">等离子切割设备</span>
            <span v-if="scope.row.mac&&scope.row.machineType === '2'">激光切割设备</span>
            <!-- {{scope.row.machineType}} -->
          </template>
        </el-table-column>
        <el-table-column 
        align="center" 
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        :show-overflow-tooltip="true" label="所属产线" min-width="100">
            <template v-slot="scope">
              <span v-if="scope.row.factory">{{scope.row.factory}}>{{scope.row.workshopInf}}>{{scope.row.productionLine}}</span>
            </template>
        </el-table-column>
        <el-table-column  
        align="center" 
       :show-overflow-tooltip="true" label="暂停切割" 
       v-if="checkPermission(permission.pauseCutting)" min-width="70">
          <template v-slot="subScope">
             <el-switch
              v-loading="subScope.row.switchLoading"
              active-color="#409EFF"
              inactive-color="#F56C6C"
              v-model="subScope.row.State"
              @change="StateChange(subScope.row,scope.row)"
              :disabled="
                !(
                  subScope.row.plateState === '2' ||
                  subScope.row.plateState === '5'
                )
              "
            />
            </template>
        </el-table-column>
         <el-table-column align="center" :show-overflow-tooltip="true" label="操作" min-width="70" v-if="checkPermission(permission.detailDelete)">
           <common-button type="danger" size="mini" v-if="scope.row.plateState !== '3'||scope.row.plateState !== '4'" @click="del(scope.row)">删除</common-button>
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
          <span>{{scope.row.projectNumber}}-{{ scope.row.projectName }}</span>
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
        v-if="columns.visible('cuttingNum')"
        prop="cuttingNum"
        key="cuttingNum"
        align="center"
        label="已切割"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.cuttingNum }}</span>
        </template>
      </el-table-column>
      <el-table-column
         v-if="columns.visible('noCuttingNum')"
        prop="noCuttingNum"
        key="noCuttingNum"
        align="center"
        label="未切割"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.plateNum-scope.row.cuttingNum}}</span>
        </template>
      </el-table-column>
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
    <!-- <task-schedul :detail-data="detailObj" v-model:visible="quicklyAssignVisible"></task-schedul> -->
    <detail-result @change="handleChangeDetail" :detail-data="detailObj" v-model:visible="specsVisible" />
  </div>
</template>

<script setup>
import useVisible from '@compos/use-visible'
import { defineProps, defineEmits,ref,watch } from 'vue'
import crudApi from '@/api/cutting/nestList'
import { get } from '@/api/cutting/project-data'
import { getMac }  from '@/api/cutting/machine'
// import crudApi2 from '@/api/cutting/machine-part'

import { suspendTask } from '@/api/cutting/scheduling'
import { continueTask } from '@/api/cutting/project-data'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import detail from '@/views/cutting/template/steel-plate-list.vue'
import detailResult from '@/views/cutting/template/detail.vue'
import {steelPlateEnum} from '@enum-ms/cutting'
// import taskSchedul from './module/task-scheduling'
import useMaxHeight from '@compos/use-max-height'
import checkPermission from '@/utils/system/check-permission'
import { ElNotification } from 'element-plus'
// import { nestWorkListPM as permission } from '@/page-permission/cutting'
import { projectTaskingPM as permission } from '@/page-permission/cutting'

const tableRef = ref()
const innerVisible = ref(false)
const detailObj = ref({})
// 钢板清单
const dataList = ref([])
const loadingList = ref(false)
const specsVisible = ref(false)

const plateData = ref([])
const switchLoading =ref(false)
const selectLineId = ref('')
const machineName = ref('')
const tabLoading = ref(false)
const currentRow=ref({})
// const machineVisible = ref(false)

const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
    detailData: {
    type: Object
  }
})

const emit = defineEmits(['update:visible', 'colesHook'])
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', closeHook, showHook })

function showHook() {
  // if (props.detailData) {
  //   console.log(props.detailData,'111111111');
  //   machineName.value = props.detailData.cutMachine.machineName
 StateChange()

  // }
}

function closeHook() {
  emit('colesHook')
}


// 重新请求钢板的接口
async function plateDataGet(row) {
  tabLoading.value = true
  try {
    const { content } = await get({mac:row.mac,projectId:row.projectId})
    content.forEach(item => {
      if (item.plateState == '5') {
        item.State = false
      } else {
        item.State = true
      }
    })
    // plateData.value  = content
     crud.data[row.rowIndex].subList = content
    // dataList.value[row.index] = content
  } catch (err) {
    console.log('钢板清单页面接口报错', err)
  }
  tabLoading.value = false
}

// 暂停切割
async function StateChange(row,parentRow) {
  // 当前行和未展开之前的行
  console.log(row,parentRow)
  row.switchLoading = true
  try {
    const data = [row.id]
    console.log(data);
    let message = ''
    if (row.plateState === '5') {
      // 继续
      message = await continueTask(data)
      
    } else {
      // 暂停
      message = await suspendTask(data)
      
    }
    ElNotification({ title: '更改状态成功', message: message, type: 'success' })
    // 重新调用钢板的接口
    console.log(currentRow.value,'11111111111')
    const list = await get({mac:parentRow.mac,projectId:parentRow.projectId})
    list.content.forEach(item => {
      if (item.plateState === '5') {
        item.State = false
      } else {
        item.State = true
      }
    })
    // 通过mac地址查机器设备
      for(var i = 0; i < list.content.length; i++) { 
          if(list.content[i].mac) {
              const macData = await getMac(list.content[i].mac)
              // if(macData !== null) {
                list.content[i].machineType = macData.machineType
                list.content[i].factory = macData.factory
                list.content[i].workshopInf = macData.workshopInf
                list.content[i].productionLine = macData.productionLine
                // }
              
              
          }
      }
    // plateData.value  = list.content
     crud.data[parentRow.rowIndex].subList = list.content
    // crud.toQuery()
  } catch (err) {
    console.log(err)
  }
  row.switchLoading = false
}
const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud, CRUD, columns } = useCRUD(
  {
    title: '项目任务',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true,
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
  currentRow.value = row
  row.loadingList = true
  try {

    const data = await get({
      // nestingState:row.nestingState,
      projectId:currentRow.value.projectId,
      mac:currentRow.value.mac
    
    })
    data.content.forEach(item => {
      if (item.plateState === '5') {
        item.State = false
      } else {
        item.State = true
      }
    })
    // const macData = await getMac('1')
    // data.content.map(v=>{
    //   v.machineType=macData.machineType
    // })
      // dataList.value[row.index] = data.content

      // 通过mac地址查询机器设备
      for(var i = 0; i < data.content.length; i++) { 
          if(data.content[i].mac) {
              const macData = await getMac(data.content[i].mac)
              // if(macData !== null) {
                data.content[i].machineType = macData.machineType
                data.content[i].factory = macData.factory
                data.content[i].workshopInf = macData.workshopInf
                data.content[i].productionLine = macData.productionLine
                // }
              
              
          }
      }

      row.subList = data.content
      // plateData.value = data.content
  } catch (error) {
    console.log('请求接口数据失败')
  }
  row.loadingList = false
}
// 删除
function del(row) {
  console.log(row) 
}
// 点击厚度查看套料成果
function nestResults(row) {
  detailObj.value = row
  selectLineId.value = row.mac
  specsVisible.value = true
}


function handleChangeDetail(row) {
  selectLineId.value = row.mac
  
}

// function endEvent() {
//   machineVisible.value = false
//   plateDataGet()
// }

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

function taskScheduling(row) {
  detailObj.value = row
  quicklyAssignVisible.value = true
  
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