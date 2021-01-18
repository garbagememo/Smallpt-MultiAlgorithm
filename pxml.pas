program pXML;
{$mode objfpc}{$H+}
uses SysUtils,Classes,uXML,uModel,uVect;

var
   SR  : SnapRecord;
   ScR : SceneRecord;
   spl : TList;
   cam : CameraRecord;
   pSt : string;
  i:integer;
BEGIN
   InitScene;//SR.InitSceneにするべき？
   SR.MakeSnap;
   pSt:='xml';
   If Not DirectoryExists(pSt) then
      If Not CreateDir (pSt) Then
	 Writeln ('Failed to create directory !')
      else
	 Writeln ('Created "NewDir" directory');
  for i:=0 to sc.count-1 do begin
    cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),640,480,0.5135,140);
     ScR.Cam:=cam;
    ScR.spl:=TList(sc[i]);
    WriteXMLScene(ScR,pst+'/'+IntToStr(i)+'Model.xml');
  end;
   Sr.GetNextScene(640,480);
   writeXMLScene(SR.CurSceneRec,pSt+'/'+'teste.xml');
   writeln(' Write XML');
   ScR:=ReadXMLConf(pSt+'/'+'teste.xml');
   writeln(' Read XML');
   writeXMLScene(ScR,'testf.xml'); 
end.
